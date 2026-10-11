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
import controllers.actions.{DataRetrievalAction, IdentifierAction}
import models.UserAnswers
import models.response.GetLastSubmittedVerificationBatchResponse
import models.verify.VerificationBatchStatus
import pages.verify.LastSubmittedVerificationBatchResponsePage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import services.{CheckLatestSubmissionStatusService, CisManageService, SubmissionStatusCheckResult, VerificationService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CheckVerificationResultsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  val controllerComponents: MessagesControllerComponents,
  verificationService: VerificationService,
  override protected val cisManageService: CisManageService,
  override protected val sessionRepository: SessionRepository
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with AgentClientChecks
    with Logging {

  def onPageLoad(): Action[AnyContent] =
    (identify andThen getData).async { implicit request =>
      val userAnswers = request.userAnswers.getOrElse(UserAnswers(request.userId))

      withAgentClientChecks(request.userId, request.isAgent, userAnswers)
        .flatMap {
          case Left(redirect)        => Future.successful(redirect)
          case Right(checkedAnswers) =>
            verificationService
              .getLastSubmittedVerificationBatch(checkedAnswers)
              .map { updatedAnswers =>
                updatedAnswers.get(LastSubmittedVerificationBatchResponsePage) match {
                  case None           =>
                    logger.error(
                      "[CheckVerificationResultsController][onPageLoad] - LastSubmittedVerificationBatchResponsePage missing from userAnswers"
                    )
                    Redirect(controllers.routes.SystemErrorController.onPageLoad())
                  case Some(response) =>
                    routeFromResponse(response)
                }
              }
        }
        .recover { case t =>
          logger.error(
            "[CheckVerificationResultsController.onPageLoad] Failed to retrieve last submitted verification batch",
            t
          )
          Redirect(controllers.routes.SystemErrorController.onPageLoad())
        }
    }

  private def routeFromResponse(response: GetLastSubmittedVerificationBatchResponse): Result = {
    val batchIdMissing = response.verificationBatch.map(_.verificationBatchId).isEmpty
    val status         = response.verificationBatch
      .flatMap(_.status)
      .flatMap { raw =>
        val parsed = VerificationBatchStatus.from(raw)
        if (parsed.isEmpty) {
          logger.warn(
            s"[CheckVerificationResultsController.onPageLoad] Unrecognised verification batch status: $raw"
          )
        }
        parsed
      }

    val showNoResults =
      batchIdMissing ||
        CheckLatestSubmissionStatusService.check(status) == SubmissionStatusCheckResult.ShowPendingVerificationWarning

    if (showNoResults) {
      Redirect(controllers.verify.routes.NoVerificationResultsController.onPageLoad())
    } else {
      Redirect(controllers.verify.routes.VerificationResultsController.onPageLoad())
    }
  }
}
