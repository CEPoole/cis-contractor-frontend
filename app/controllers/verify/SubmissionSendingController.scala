/*
 * Copyright 2026 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package controllers.verify

import config.FrontendAppConfig
import controllers.actions.*
import models.verify.SubmissionStatus
import models.verify.SubmissionStatus.*
import models.response.{ChrisPollResponse, ChrisSubmissionResponse}
import models.verify.GovTalkErrorStatus.{DepartmentalError, FatalError}
import pages.verify.VerificationSubmissionDetailsPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request, Result}
import services.VerificationService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import views.html.verify.SubmissionSendingView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SubmissionSendingController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  reconcileFormpRds: FormpRdsReconcileAction,
  val controllerComponents: MessagesControllerComponents,
  appConfig: FrontendAppConfig,
  view: SubmissionSendingView,
  verificationService: VerificationService
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val SubmitAgainErrorCodes = Set("3000", "1000", "2005")

  private def recovery: Result =
    Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  def onPageLoad: Action[AnyContent] =
    (identify andThen getData andThen requireData andThen reconcileFormpRds).async { implicit request =>
      implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      verificationService.createSubmitAndPersistVerificationSubmission
        .flatMap(redirectForInitialSubmissionResponse)
        .recover { case ex =>
          logger.error(
            "[SubmissionSendingController.onPageLoad] Failed to create submission",
            ex
          )
          recovery
        }
    }

  def onPollAndRedirect: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      request.userAnswers.get(VerificationSubmissionDetailsPage) match {
        case None =>
          logger.error(
            "[SubmissionSendingController][onPollAndRedirect] - VerificationSubmissionDetailsPage missing from userAnswers"
          )
          Future.successful(recovery)

        case Some(submissionDetails) =>
          val gatePollInterval =
            submissionDetails.pollIntervalSeconds
              .getOrElse(appConfig.submissionPollDefaultIntervalSeconds)
          // Fixed page-refresh cadence, independent of the ChRIS poll interval used for the gate.
          val refreshInterval  = appConfig.submissionPollDefaultIntervalSeconds

          if (!verificationService.isPollDue(submissionDetails, gatePollInterval)) {
            Future.successful(
              Ok(view())
                .withHeaders("Refresh" -> refreshInterval.toString)
            )
          } else {
            verificationService
              .pollStatusAndPersist(request.userAnswers, submissionDetails)
              .flatMap { response =>
                redirectForPollSubmissionResponse(response, refreshInterval)
              }
              .recover { case ex =>
                logger.error(
                  "[SubmissionSendingController.onPollAndRedirect] Verification poll failed",
                  ex
                )
                recovery
              }
          }
      }
    }

  private def redirectForErrorStatus(
    status: SubmissionStatus,
    govTalkErrorStatus: Option[models.verify.GovTalkErrorStatus]
  ): Result =
    status match {

      case DEPARTMENTAL_ERROR if isSubmitAgainError(govTalkErrorStatus) =>
        Redirect(
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
        )

      case DEPARTMENTAL_ERROR =>
        Redirect(
          controllers.verify.routes.VerifyDepartmentalErrorController
            .onPageLoad()
        )

      case FATAL_ERROR if isSubmitAgainError(govTalkErrorStatus) =>
        Redirect(
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
        )

      case FATAL_ERROR =>
        Redirect(
          controllers.verify.routes.VerificationNotSubmittedWarningController
            .onPageLoad()
        )

      case unexpectedStatus =>
        logger.error(
          "[SubmissionSendingController][redirectForErrorStatus] - " +
            s"unexpected submission status=${unexpectedStatus.value}"
        )
        recovery
    }

  private def redirectForInitialSubmissionResponse(
    response: ChrisSubmissionResponse
  ): Future[Result] =
    SubmissionStatus.fromString(response.status) match {

      case SubmissionStatus.PENDING | SubmissionStatus.ACCEPTED =>
        Future.successful(
          Redirect(controllers.verify.routes.SubmissionSendingController.onPollAndRedirect)
        )

      case SUBMITTED_NO_RECEIPT =>
        Future.successful(
          Redirect(controllers.verify.routes.VerificationRequestSubmittedController.onPageLoad())
        )

      case status @ (DEPARTMENTAL_ERROR | FATAL_ERROR) =>
        Future.successful(redirectForErrorStatus(status, response.govTalkErrorStatus))

      case unexpectedStatus =>
        logger.error(
          "[SubmissionSendingController][redirectForInitialSubmissionResponse] - " +
            s"unexpected submission status=${unexpectedStatus.value}"
        )
        Future.successful(recovery)
    }

  private def redirectForPollSubmissionResponse(
    response: ChrisPollResponse,
    refreshInterval: Int
  )(implicit request: Request[_]): Future[Result] =
    response.status match {
      case SubmissionStatus.PENDING | SubmissionStatus.ACCEPTED =>
        Future.successful(
          Ok(view())
            .withHeaders("Refresh" -> refreshInterval.toString)
        )

      case SUBMITTED =>
        Future.successful(
          Redirect(controllers.verify.routes.VerificationRequestSubmittedController.onPageLoad())
        )

      case SUBMITTED_NO_RECEIPT =>
        Future.successful(
          Redirect(controllers.verify.routes.VerificationRequestSubmittedController.onPageLoad())
        )

      case status @ (DEPARTMENTAL_ERROR | FATAL_ERROR) =>
        Future.successful(redirectForErrorStatus(status, response.govTalkErrorStatus))

      case SEND_ERROR =>
        Future.successful(
          Redirect(
            controllers.verify.routes.VerifySendErrorController.onPageLoad()
          )
        )

      case TIMED_OUT => Future.successful(Redirect(controllers.verify.routes.VerifySendErrorController.onPageLoad()))

      case unexpectedStatus =>
        logger.error(
          "[SubmissionSendingController][redirectForPollSubmissionResponse] - " +
            s"unexpected poll status=${unexpectedStatus.value}"
        )
        Future.successful(recovery)
    }

  private def isSubmitAgainError(
    govTalkErrorStatus: Option[models.verify.GovTalkErrorStatus]
  ): Boolean =
    govTalkErrorStatus.exists {
      case FatalError(errorCode, _) =>
        SubmitAgainErrorCodes.contains(errorCode)

      case DepartmentalError(Some(errorCode), _) =>
        SubmitAgainErrorCodes.contains(errorCode)

      case _ =>
        false
    }
}
