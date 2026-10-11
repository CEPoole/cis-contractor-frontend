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

import controllers.actions.*
import forms.verify.ContractorEmailConfirmationStoredFormProvider
import models.{Mode, UserAnswers}
import models.finalvalidation.{FinalValidationDraftRequestBuilder, VerifyFinalValidationContinuation}
import navigation.Navigator
import pages.finalvalidation.{FinalValidationDraftIdPage, VerifyFinalValidationContinuationPage, VerifyFinalValidationModePage}
import pages.verify.{ContractorEmailConfirmationNotStoredPage, ContractorEmailConfirmationStoredPage, NewestVerificationBatchResponsePage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.VerifyFinalValidationService
import services.finalvalidation.FinalValidationDraftService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.verify.ContractorEmailConfirmationStoredView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ContractorEmailConfirmationStoredController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  requireCisId: CisIdRequiredAction,
  verifyFinalValidationService: VerifyFinalValidationService,
  finalValidationDraftService: FinalValidationDraftService,
  finalValidationDraftRequestBuilder: FinalValidationDraftRequestBuilder,
  formProvider: ContractorEmailConfirmationStoredFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: ContractorEmailConfirmationStoredView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  private def recoveryRedirect = Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  private def emailNotStoredRedirect(mode: Mode) = Redirect(
    controllers.verify.routes.ContractorEmailConfirmationNotStoredController.onPageLoad(mode)
  )

  private def emailNotStoredAfterFinalValidationRedirect(mode: Mode) = Redirect(
    controllers.verify.routes.ContractorEmailConfirmationNotStoredController.onPageLoadAfterFinalValidation(mode)
  )

  private def preparedForm(userAnswers: UserAnswers) =
    userAnswers.get(ContractorEmailConfirmationStoredPage).fold(form)(form.fill)

  private def getEmailAddress(userAnswers: UserAnswers): Either[Unit, Option[String]] =
    userAnswers.get(NewestVerificationBatchResponsePage) match {
      case None           => Left(())
      case Some(response) => Right(response.scheme.flatMap(_.emailAddress))
    }

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>
      getEmailAddress(request.userAnswers) match {
        case Left(_) =>
          logger.error(
            "[ContractorEmailConfirmationStoredController][onPageLoad] - NewestVerificationBatchResponsePage missing from userAnswers"
          )
          Future.successful(recoveryRedirect)

        case Right(None) =>
          Future.successful(emailNotStoredRedirect(mode))

        case Right(Some(email)) =>
          for {
            validation <- verifyFinalValidationService.validate(
                            request.cisId,
                            request.userAnswers
                          )

            result <-
              if (validation.hasErrors) {
                for {
                  createRequest <- Future.fromTry(
                                     finalValidationDraftRequestBuilder.build(
                                       request.cisId,
                                       validation
                                     )
                                   )

                  draftId <- finalValidationDraftService.create(createRequest)

                  withContinuation <- Future.fromTry(
                                        request.userAnswers.set(
                                          VerifyFinalValidationContinuationPage,
                                          VerifyFinalValidationContinuation.ContractorEmailConfirmationStored
                                        )
                                      )

                  withDraftId <- Future.fromTry(
                                   withContinuation.set(
                                     FinalValidationDraftIdPage,
                                     draftId
                                   )
                                 )

                  withMode <- Future.fromTry(
                                withDraftId.set(
                                  VerifyFinalValidationModePage,
                                  mode.toString
                                )
                              )

                  _ <- sessionRepository.set(withMode)

                } yield Redirect(
                  controllers.finalvalidations.routes.ReviewSubcontractorDetailsController.onPageLoad()
                )
              } else {
                Future.successful(
                  Ok(view(preparedForm(request.userAnswers), mode, email))
                )
              }

          } yield result
      }
    }

  def onPageLoadAfterFinalValidation(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      getEmailAddress(request.userAnswers) match {
        case Left(_) =>
          logger.error(
            "[ContractorEmailConfirmationStoredController][onPageLoadAfterFinalValidation] - NewestVerificationBatchResponsePage missing from userAnswers"
          )
          Future.successful(recoveryRedirect)

        case Right(None) =>
          Future.successful(emailNotStoredAfterFinalValidationRedirect(mode))

        case Right(Some(email)) =>
          for {
            updatedAnswers <- Future.fromTry(
                                request.userAnswers.remove(
                                  VerifyFinalValidationContinuationPage
                                )
                              )

            _ <- sessionRepository.set(updatedAnswers)

          } yield Ok(
            view(
              preparedForm(updatedAnswers),
              mode,
              email
            )
          )
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      getEmailAddress(request.userAnswers).toOption.flatten
        .fold {
          logger.error(
            "[ContractorEmailConfirmationStoredController][onSubmit] - NewestVerificationBatchResponsePage or scheme email missing"
          )
          Future.successful(recoveryRedirect)
        } { emailAddress =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, emailAddress))),
              value =>
                for {
                  cleanedAnswers <-
                    Future.fromTry(request.userAnswers.remove(ContractorEmailConfirmationNotStoredPage))
                  updatedAnswers <-
                    Future.fromTry(cleanedAnswers.set(ContractorEmailConfirmationStoredPage, value))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(ContractorEmailConfirmationStoredPage, mode, updatedAnswers))
            )
        }
  }
}
