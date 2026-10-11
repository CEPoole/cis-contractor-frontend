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
import models.{CheckMode, Mode, NormalMode, UserAnswers}
import models.finalvalidation.VerifyFinalValidationSource.*
import models.finalvalidation.*
import models.RichJsObject
import pages.finalvalidation.*
import pages.verify.{SelectSubcontractorPage, SelectSubcontractorsToReverifyPage}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import play.api.Logging

import javax.inject.{Inject, Singleton}
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.libs.json.*
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.finalvalidation.FinalValidationDraftService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import views.html.finalvalidations.ReviewSubcontractorDetailsView

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

@Singleton
class ReviewSubcontractorDetailsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  requireCisId: CisIdRequiredAction,
  sessionRepository: SessionRepository,
  finalValidationDraftService: FinalValidationDraftService,
  pageModelBuilder: UpdateSubcontractorDetailsPageModelBuilder,
  val controllerComponents: MessagesControllerComponents,
  view: ReviewSubcontractorDetailsView
)(using ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>

      implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      (
        request.userAnswers.get(FinalValidationDraftIdPage),
        request.userAnswers.get(VerifyFinalValidationSourcePage),
        request.userAnswers.get(VerifyFinalValidationModePage).flatMap(modeFromString)
      ) match {

        case (Some(draftId), Some(source), Some(mode)) =>
          finalValidationDraftService.get(request.cisId, draftId).map { draft =>

            val rows =
              draft.subcontractors.map { subcontractor =>
                ReviewSubcontractorDetailsRow(
                  subcontractorId = subcontractor.subcontractorId,
                  name = pageModelBuilder.displayName(subcontractor),
                  hasErrors = subcontractor.readiness == FinalValidationReadiness.Incomplete
                )
              }

            Ok(
              view(
                ReviewSubcontractorDetailsPageModel(
                  subcontractors = rows,
                  canContinue = draft.allComplete,
                  backUrl = backUrl(source, mode)
                )
              )
            )
          }

        case _ =>
          logger.error("[ReviewSubcontractorDetailsController][onPageLoad] - required session data missing")
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )
      }
    }

  def onSubmit: Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>

      implicit val hc: HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      (
        request.userAnswers.get(FinalValidationDraftIdPage),
        request.userAnswers.get(VerifyFinalValidationContinuationPage),
        request.userAnswers.get(VerifyFinalValidationModePage).flatMap(modeFromString)
      ) match {

        case (Some(draftId), Some(continuation), Some(mode)) =>
          finalValidationDraftService.get(request.cisId, draftId).flatMap { draft =>
            if (!draft.allComplete) {
              Future.successful(
                Redirect(
                  controllers.finalvalidations.routes.ReviewSubcontractorDetailsController.onPageLoad()
                )
              )
            } else {
              finalValidationDraftService.commit(request.cisId, draftId).flatMap { _ =>

                val answersWithCorrectedNames =
                  syncSelectedSubcontractorNames(request.userAnswers, draft)

                val cleanedAnswers =
                  for {
                    withCorrectedNames  <- answersWithCorrectedNames
                    withoutDraftId      <- withCorrectedNames.remove(FinalValidationDraftIdPage)
                    withoutSource       <- withoutDraftId.remove(VerifyFinalValidationSourcePage)
                    withoutMode         <- withoutSource.remove(VerifyFinalValidationModePage)
                    withoutContext      <- withoutMode.remove(FinalValidationContextPage)
                    withoutPayload      <- withoutContext.remove(VerifyFinalValidationPayloadPage)
                    withoutChangeTarget <- withoutPayload.remove(FinalValidationChangeTargetPage)
                    withoutBaseUtr      <- withoutChangeTarget.remove(FinalValidationBaseUtrPage)
                    withoutContinuation <- withoutBaseUtr.remove(VerifyFinalValidationContinuationPage)
                  } yield withoutContinuation

                Future.fromTry(cleanedAnswers).flatMap { answers =>
                  sessionRepository.set(answers).map { _ =>
                    continuation match {
                      case VerifyFinalValidationContinuation.ContractorEmailConfirmationStored =>
                        Redirect(
                          controllers.verify.routes.ContractorEmailConfirmationStoredController
                            .onPageLoadAfterFinalValidation(mode)
                        )

                      case VerifyFinalValidationContinuation.ContractorEmailConfirmationNotStored =>
                        Redirect(
                          controllers.verify.routes.ContractorEmailConfirmationNotStoredController
                            .onPageLoadAfterFinalValidation(mode)
                        )

                      case _ =>
                        logger.error("[ReviewSubcontractorDetailsController][onSubmit] - unexpected continuation")
                        Redirect(
                          controllers.routes.JourneyRecoveryController.onPageLoad()
                        )
                    }
                  }
                }
              }
            }
          }

        case _ =>
          logger.error("[ReviewSubcontractorDetailsController][onSubmit] - required session data missing")
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )
      }
    }

  private def backUrl(
    source: VerifyFinalValidationSource,
    mode: Mode
  ): String =
    source match {
      case SelectSubcontractor =>
        controllers.verify.routes.SelectSubcontractorController.onPageLoad(mode).url

      case SelectSubcontractorsToReverify =>
        controllers.verify.routes.SelectSubcontractorsToReverifyController.onPageLoad(mode).url

      case ReviewUnmatchedSubcontractors =>
        controllers.verify.routes.ReviewUnmatchedSubcontractorsController.onPageLoad().url

      case ReviewInsufficientInfoSubcontractors =>
        controllers.verify.routes.ReviewInsufficientInfoSubcontractorsController.onPageLoad().url
    }

  private def modeFromString(value: String): Option[Mode] =
    value match {
      case "NormalMode" => Some(NormalMode)
      case "CheckMode"  => Some(CheckMode)
      case _            => None
    }

  private def syncSelectedSubcontractorNames(
    answers: UserAnswers,
    draft: FinalValidationDraft
  )(implicit messages: Messages): Try[UserAnswers] = {

    val correctedNames =
      draft.subcontractors.map { subcontractor =>
        subcontractor.subcontractorId.toString ->
          pageModelBuilder.displayName(subcontractor)
      }.toMap

    for {
      withSelectedSubcontractors <-
        answers
          .get(SelectSubcontractorPage)
          .map { selectedSubcontractors =>
            val updatedSelectedSubcontractors =
              selectedSubcontractors.map { subcontractor =>
                correctedNames
                  .get(subcontractor.id)
                  .map(name => subcontractor.copy(name = name))
                  .getOrElse(subcontractor)
              }

            updatePageData(
              answers,
              SelectSubcontractorPage.path,
              updatedSelectedSubcontractors
            )
          }
          .getOrElse(
            Success(answers)
          )

      withReverifySubcontractors <-
        withSelectedSubcontractors
          .get(SelectSubcontractorsToReverifyPage)
          .map { selectedSubcontractors =>
            val updatedSelectedSubcontractors =
              selectedSubcontractors.map { subcontractor =>
                correctedNames
                  .get(subcontractor.id)
                  .map(name => subcontractor.copy(name = name))
                  .getOrElse(subcontractor)
              }

            updatePageData(
              withSelectedSubcontractors,
              SelectSubcontractorsToReverifyPage.path,
              updatedSelectedSubcontractors
            )
          }
          .getOrElse(
            Success(withSelectedSubcontractors)
          )
    } yield withReverifySubcontractors
  }

  private def updatePageData[A: Writes](
    answers: UserAnswers,
    path: JsPath,
    value: A
  ): Try[UserAnswers] =
    answers.data.setObject(path, Json.toJson(value)) match {
      case JsSuccess(updatedData, _) =>
        Success(answers.copy(data = updatedData))
      case JsError(errors)           =>
        Failure(JsResultException(errors))
    }

}
