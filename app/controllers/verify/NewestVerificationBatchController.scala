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

import controllers.AgentClientChecks
import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import models.{NormalMode, UserAnswers}
import models.verify.VerificationBatchStatus
import pages.verify.{NewestVerificationBatchResponsePage, ReverificationDecisionsPage, UnverifiedSubcontractorsPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{CheckLatestSubmissionStatusService, CheckUnmatchedSubcontractorsService, CisManageService, SubmissionStatusCheckResult, VerificationService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import java.time.{Clock, LocalDateTime}

class NewestVerificationBatchController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  verificationBatchService: VerificationService,
  clock: Clock,
  override protected val cisManageService: CisManageService,
  override protected val sessionRepository: SessionRepository
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with AgentClientChecks
    with Logging {

  private sealed trait InactivityStatus
  private object InactivityStatus {
    case object Active extends InactivityStatus
    case object Inactive extends InactivityStatus
    case object MissingData extends InactivityStatus
    val SixMonths: Long = 6
  }

  private def checkSchemeInactivity(response: models.response.GetNewestVerificationBatchResponse): InactivityStatus =
    response.monthlyReturn match {
      case None =>
        // No monthly return submitted - scheme is active
        InactivityStatus.Active

      case Some(monthlyReturn) if !monthlyReturn.decNoMoreSubPayments.contains("Y") =>
        InactivityStatus.Active

      case Some(_) =>
        response.monthlyReturnSubmission match {
          case None =>
            InactivityStatus.MissingData

          case Some(monthlyReturnSubmission) =>
            monthlyReturnSubmission.submissionRequestDate match {
              case Some(requestDate) =>
                val sixMonthsLater = requestDate.plusMonths(InactivityStatus.SixMonths)
                val now            = LocalDateTime.now(clock)
                if (!now.isAfter(sixMonthsLater)) InactivityStatus.Inactive else InactivityStatus.Active
              case None              =>
                InactivityStatus.MissingData
            }
        }
    }

  private def checkSubmissionStatus(
    response: models.response.GetNewestVerificationBatchResponse
  ): SubmissionStatusCheckResult =
    response.verificationBatch.flatMap(_.status) match {
      case Some(rawStatus) =>
        VerificationBatchStatus.from(rawStatus) match {
          case Some(status) =>
            CheckLatestSubmissionStatusService.check(Some(status))

          case None =>
            logger.warn(
              s"[NewestVerificationBatchController] Unrecognised verification batch status: $rawStatus"
            )
            CheckLatestSubmissionStatusService.check(None)
        }

      case None =>
        CheckLatestSubmissionStatusService.check(None)
    }

  private def routeFromResponse(
    updatedAnswers: UserAnswers,
    response: models.response.GetNewestVerificationBatchResponse,
    unverified: Seq[models.Subcontractor]
  ): Future[play.api.mvc.Result] =
    checkSchemeInactivity(response) match {
      case InactivityStatus.MissingData =>
        logger.error(
          "[NewestVerificationBatchController][routeFromResponse] - monthlyReturnSubmission data missing for scheme"
        )
        Future.successful(
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        )

      case InactivityStatus.Inactive =>
        Future.successful(
          Redirect(
            controllers.verify.routes.InactiveSchemeWarningController
              .onPageLoad()
          )
        )

      case InactivityStatus.Active =>
        routeFromSubmissionStatus(
          updatedAnswers,
          response,
          unverified
        )
    }

  private def routeFromSubmissionStatus(
    updatedAnswers: UserAnswers,
    response: models.response.GetNewestVerificationBatchResponse,
    unverified: Seq[models.Subcontractor]
  ): Future[play.api.mvc.Result] =
    checkSubmissionStatus(response) match {
      case SubmissionStatusCheckResult.ShowPendingVerificationWarning =>
        Future.successful(
          Redirect(
            controllers.verify.routes.VerificationRequestInProgressController
              .onPageLoad()
          )
        )

      case SubmissionStatusCheckResult.CheckUnmatchedSubcontractors =>
        val decisions =
          CheckUnmatchedSubcontractorsService.reverificationDecisions(
            response.verifications,
            response.subcontractors
          )

        for {
          answersWithDecisions <- Future.fromTry(
                                    updatedAnswers.set(
                                      ReverificationDecisionsPage,
                                      decisions
                                    )
                                  )
          _                    <- sessionRepository.set(answersWithDecisions)
        } yield
          if (decisions.exists(_.considerForReverification)) {
            Redirect(
              controllers.routes.UnmatchedSubcontractorsController.onPageLoad()
            )
          } else {
            continueToF4(response, unverified)
          }

      case SubmissionStatusCheckResult.Continue =>
        Future.successful(
          continueToF4(response, unverified)
        )
    }

  private def continueToF4(
    response: models.response.GetNewestVerificationBatchResponse,
    unverified: Seq[models.Subcontractor]
  ): play.api.mvc.Result =
    if (response.subcontractors.isEmpty) {
      Redirect(controllers.verify.routes.NoSubcontractorsAddedController.onPageLoad())
    } else if (unverified.isEmpty) {
      Redirect(controllers.verify.routes.VerifyYourSubcontractorsYesNoController.onPageLoad)
    } else {
      Redirect(controllers.verify.routes.SelectSubcontractorController.onPageLoad(NormalMode))
    }

  def onPageLoad(): Action[AnyContent] =
    (identify andThen getData).async { implicit request =>
      val userAnswers = request.userAnswers.getOrElse(UserAnswers(request.userId))

      withAgentClientChecks(request.userId, request.isAgent, userAnswers)
        .flatMap {
          case Left(redirect)        => Future.successful(redirect)
          case Right(checkedAnswers) =>
            verificationBatchService
              .refreshNewestVerificationBatch(checkedAnswers)
              .flatMap { updatedAnswers =>

                val batch      = updatedAnswers.get(NewestVerificationBatchResponsePage)
                val unverified = updatedAnswers.get(UnverifiedSubcontractorsPage).getOrElse(Seq.empty)

                batch match {
                  case None =>
                    logger.error(
                      "[NewestVerificationBatchController][onPageLoad] - NewestVerificationBatchResponsePage missing from userAnswers"
                    )
                    Future.successful(
                      Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
                    )

                  case Some(response) =>
                    routeFromResponse(
                      updatedAnswers,
                      response,
                      unverified
                    )
                }
              }
        }
        .recover { case t =>
          logger.error(
            "[NewestVerificationBatchController.onPageLoad] Failed to refresh newest verification batch",
            t
          )
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }

  def onContinue: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      verificationBatchService
        .refreshNewestVerificationBatch(request.userAnswers)
        .flatMap { updatedAnswers =>

          val batch      = updatedAnswers.get(NewestVerificationBatchResponsePage)
          val unverified = updatedAnswers.get(UnverifiedSubcontractorsPage).getOrElse(Seq.empty)

          batch match {
            case None =>
              logger.error(
                "[NewestVerificationBatchController][onContinue] - NewestVerificationBatchResponsePage missing from userAnswers"
              )
              Future.successful(
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              )

            case Some(response) =>
              routeFromSubmissionStatus(
                updatedAnswers,
                response,
                unverified
              )
          }
        }
        .recover { case t =>
          logger.error(
            "[NewestVerificationBatchController.onContinue] Failed to refresh newest verification batch",
            t
          )
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }
}
