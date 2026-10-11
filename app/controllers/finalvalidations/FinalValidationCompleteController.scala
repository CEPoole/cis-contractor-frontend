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

package controllers.finalvalidations

import config.FrontendAppConfig
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import services.FinalValidationHandoffService
import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import models.finalvalidation.{FinalValidationContext, FinalValidationCorrectionBuilder}
import models.requests.DataRequest
import pages.finalvalidation.{FinalValidationContextPage, FinalValidationDraftIdPage, FinalValidationHandoffPage, VerifyFinalValidationPayloadPage}
import play.api.Logging
import services.finalvalidation.FinalValidationDraftService

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class FinalValidationCompleteController @Inject() (
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  finalValidationHandoffService: FinalValidationHandoffService,
  finalValidationDraftService: FinalValidationDraftService,
  correctionBuilder: FinalValidationCorrectionBuilder,
  appConfig: FrontendAppConfig,
  val controllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with Logging {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData).async { implicit request =>
    request.userAnswers.get(FinalValidationContextPage) match {
      case Some(FinalValidationContext.VerifySubcontractor) =>
        completeVerify()

      case Some(FinalValidationContext.MonthlyReturn) =>
        completeMonthlyReturn()

      case None =>
        logger.error("[FinalValidationCompleteController][onPageLoad] - FinalValidationContextPage missing")
        Future.successful(
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        )
    }
  }

  private def completeVerify()(implicit request: DataRequest[?]): Future[Result] =
    (
      request.userAnswers.get(VerifyFinalValidationPayloadPage),
      request.userAnswers.get(FinalValidationDraftIdPage)
    ) match {

      case (Some(payload), Some(draftId)) if payload.draftId == draftId =>
        for {
          correction <- Future.fromTry(
                          correctionBuilder.build(
                            request.userAnswers,
                            payload
                          )
                        )
          _          <- finalValidationDraftService.updateCorrection(
                          instanceId = payload.instanceId,
                          draftId = draftId,
                          correction = correction
                        )
        } yield Redirect(
          controllers.finalvalidations.routes.UpdateSubcontractorDetailsController
            .onPageLoad(payload.subcontractorId)
        )

      case (Some(payload), Some(draftId)) =>
        logger.error(
          s"Final Validation draft ID mismatch for Verify. " +
            s"Payload draftId: ${payload.draftId}, session draftId: $draftId"
        )

        Future.successful(
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        )

      case _ =>
        logger.error("[FinalValidationCompleteController][completeVerify] - payload or draftId missing")
        Future.successful(
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        )
    }

  private def completeMonthlyReturn()(implicit request: DataRequest[?]): Future[Result] =
    request.userAnswers.get(FinalValidationHandoffPage) match {

      case Some(handoffId) =>
        finalValidationHandoffService.getPayload(handoffId).flatMap {
          case Some(payload) =>
            request.userAnswers.get(FinalValidationDraftIdPage) match {

              case Some(draftId) if payload.draftId == draftId =>
                for {
                  correction <- Future.fromTry(
                                  correctionBuilder.build(request.userAnswers, payload)
                                )
                  _          <- finalValidationDraftService.updateCorrection(
                                  instanceId = payload.instanceId,
                                  draftId = draftId,
                                  correction = correction
                                )
                } yield Redirect(
                  appConfig.cisFrontendFinalValidationReturnUrl(handoffId)
                )

              case Some(draftId) =>
                logger.error(
                  s"Final Validation draft ID mismatch for Monthly Return. " +
                    s"Payload draftId: ${payload.draftId}, session draftId: $draftId"
                )

                Future.successful(
                  Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
                )

              case None =>
                logger.error("[FinalValidationCompleteController][completeMonthlyReturn] - draftId missing")
                Future.successful(
                  Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
                )
            }

          case None =>
            logger.error("[FinalValidationCompleteController][completeMonthlyReturn] - handoff payload not found")
            Future.successful(
              Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
            )
        }

      case None =>
        logger.error("[FinalValidationCompleteController][completeMonthlyReturn] - handoffId missing")
        Future.successful(
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        )
    }

}
