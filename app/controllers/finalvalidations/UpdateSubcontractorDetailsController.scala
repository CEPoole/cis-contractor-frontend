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

import controllers.actions.*
import models.finalvalidation.{FinalValidationReadiness, UpdateSubcontractorDetailsPageModel, UpdateSubcontractorDetailsPageModelBuilder}
import pages.finalvalidation.FinalValidationDraftIdPage
import services.VerifyFinalValidationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.{Inject, Singleton}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.finalvalidation.FinalValidationDraftService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import views.html.finalvalidations.UpdateSubcontractorDetailsView

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UpdateSubcontractorDetailsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  requireCisId: CisIdRequiredAction,
  finalValidationDraftService: FinalValidationDraftService,
  verifyFinalValidationService: VerifyFinalValidationService,
  pageModelBuilder: UpdateSubcontractorDetailsPageModelBuilder,
  val controllerComponents: MessagesControllerComponents,
  view: UpdateSubcontractorDetailsView
)(using ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(subcontractorId: Long): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>

      implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      request.userAnswers.get(FinalValidationDraftIdPage) match {

        case Some(draftId) =>
          finalValidationDraftService.get(request.cisId, draftId).map { draft =>
            draft.subcontractor(subcontractorId) match {

              case Some(subcontractor) if subcontractor.readiness == FinalValidationReadiness.Complete =>
                Redirect(
                  controllers.finalvalidations.routes.ReviewSubcontractorDetailsController.onPageLoad()
                )

              case Some(subcontractor) =>
                val rows =
                  pageModelBuilder.build(
                    subcontractor,
                    (field, target) =>
                      controllers.finalvalidations.routes.FinalValidationChangeController
                        .onPageLoad(
                          subcontractorId,
                          field.key,
                          target.key
                        )
                        .url
                  )

                Ok(
                  view(
                    UpdateSubcontractorDetailsPageModel(
                      subcontractor.subcontractorId,
                      pageModelBuilder.displayName(subcontractor),
                      rows
                    )
                  )
                )

              case None =>
                logger.error("[UpdateSubcontractorDetailsController][onPageLoad] - subcontractor missing from draft")
                Redirect(
                  controllers.routes.JourneyRecoveryController.onPageLoad()
                )
            }
          }

        case None =>
          logger.error("[UpdateSubcontractorDetailsController][onPageLoad] - FinalValidationDraftIdPage missing")
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )
      }
    }

  def onSubmit(subcontractorId: Long): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>

      implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      request.userAnswers.get(FinalValidationDraftIdPage) match {

        case Some(draftId) =>
          finalValidationDraftService.get(request.cisId, draftId).flatMap { draft =>
            draft.subcontractor(subcontractorId) match {

              case Some(subcontractor) if subcontractor.readiness == FinalValidationReadiness.Complete =>
                Future.successful(
                  Redirect(
                    controllers.finalvalidations.routes.ReviewSubcontractorDetailsController.onPageLoad()
                  )
                )

              case Some(_) =>
                for {
                  issues <- Future.fromTry(
                              verifyFinalValidationService.validateDraftSubcontractor(
                                draft,
                                subcontractorId
                              )
                            )
                  _      <- finalValidationDraftService.updateReadiness(
                              request.cisId,
                              draftId,
                              subcontractorId,
                              issues
                            )
                } yield Redirect(
                  controllers.finalvalidations.routes.ReviewSubcontractorDetailsController.onPageLoad()
                )

              case None =>
                logger.error("[UpdateSubcontractorDetailsController][onSubmit] - subcontractor missing from draft")
                Future.successful(
                  Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
                )
            }
          }

        case None =>
          logger.error("[UpdateSubcontractorDetailsController][onSubmit] - FinalValidationDraftIdPage missing")
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )
      }
    }

  def onCancel(subcontractorId: Long): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>
      request.userAnswers.get(FinalValidationDraftIdPage) match {

        case Some(draftId) =>
          finalValidationDraftService
            .resetSubcontractor(
              request.cisId,
              draftId,
              subcontractorId
            )
            .map { _ =>
              Redirect(routes.ReviewSubcontractorDetailsController.onPageLoad())
            }

        case None =>
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )
      }
    }

}
