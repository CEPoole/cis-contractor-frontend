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

package controllers.unmatched

import config.FrontendAppConfig
import controllers.actions.*
import forms.unmatched.RemoveSubcontractorVerifyRequestFormProvider
import models.Mode
import pages.unmatched.RemoveSubcontractorVerifyRequestPage
import pages.verify.CurrentVerificationBatchResponsePage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{CheckUnmatchedSubcontractorsService, VerificationService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.unmatched.RemoveSubcontractorVerifyRequestView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class RemoveSubcontractorVerifyRequestController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  requireCisId: CisIdRequiredAction,
  formProvider: RemoveSubcontractorVerifyRequestFormProvider,
  verificationService: VerificationService,
  val controllerComponents: MessagesControllerComponents,
  view: RemoveSubcontractorVerifyRequestView
)(implicit ec: ExecutionContext, appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form = formProvider()

  private def recoveryRedirect =
    Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  def onPageLoad(subcontractorId: Long, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>
      request.userAnswers.get(CurrentVerificationBatchResponsePage) match {
        case Some(batch) =>
          batch.verifications.find(_.subcontractorId.contains(subcontractorId)) match {

            case Some(verification) if !CheckUnmatchedSubcontractorsService.isUnmatched(verification) =>
              Redirect(controllers.verify.routes.ReviewUnmatchedSubcontractorsController.onPageLoad())

            case Some(_) =>
              batch.subcontractors
                .find(_.subcontractorId == subcontractorId)
                .map { subcontractor =>
                  val preparedForm =
                    request.userAnswers
                      .get(RemoveSubcontractorVerifyRequestPage(subcontractorId))
                      .fold(form)(form.fill)

                  Ok(view(preparedForm, subcontractor.displayName, subcontractorId))
                }
                .getOrElse {
                  logger.error(
                    "[RemoveSubcontractorVerifyRequestController][onPageLoad] - " +
                      s"subcontractor not found in CurrentVerificationBatchResponsePage, subcontractorId=$subcontractorId"
                  )
                  recoveryRedirect
                }

            case None =>
              logger.error(
                "[RemoveSubcontractorVerifyRequestController][onPageLoad] - " +
                  s"verification not found in CurrentVerificationBatchResponsePage, subcontractorId=$subcontractorId"
              )
              recoveryRedirect
          }

        case None =>
          logger.error(
            "[RemoveSubcontractorVerifyRequestController][onPageLoad] - CurrentVerificationBatchResponsePage missing from userAnswers"
          )
          recoveryRedirect
      }
    }

  def onSubmit(subcontractorId: Long, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen requireCisId).async { implicit request =>

      val result =
        request.userAnswers.get(CurrentVerificationBatchResponsePage) match {
          case Some(batch) =>
            batch.subcontractors
              .find(_.subcontractorId == subcontractorId)
              .map { subcontractor =>
                form
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      Future.successful(BadRequest(view(formWithErrors, subcontractor.displayName, subcontractorId))),
                    value =>
                      if (value) {
                        batch.verifications
                          .find(_.subcontractorId.contains(subcontractorId))
                          .flatMap(_.verificationResourceRef) match {
                          case Some(verificationResourceRef) =>
                            for {
                              updatedAnswers <-
                                Future.fromTry(
                                  request.userAnswers.set(RemoveSubcontractorVerifyRequestPage(subcontractorId), value)
                                )
                              deleteResponse <-
                                verificationService.deleteVerification(updatedAnswers, verificationResourceRef)
                            } yield
                              if (deleteResponse.verificationsCounter.exists(_ > 0)) {
                                Redirect(
                                  controllers.verify.routes.ReviewUnmatchedSubcontractorsController.onPageLoad()
                                )
                              } else if (deleteResponse.verificationsCounter.contains(0L)) {
                                Redirect(appConfig.retrieveSubcontractorListUrl)
                              } else {
                                logger.error(
                                  "[RemoveSubcontractorVerifyRequestController][onSubmit] - " +
                                    s"unexpected verificationsCounter after delete, subcontractorId=$subcontractorId"
                                )
                                recoveryRedirect
                              }

                          case None =>
                            logger.error(
                              "[RemoveSubcontractorVerifyRequestController][onSubmit] - " +
                                s"verificationResourceRef missing for subcontractorId=$subcontractorId"
                            )
                            Future.successful(recoveryRedirect)
                        }
                      } else {
                        for {
                          updatedAnswers <-
                            Future.fromTry(
                              request.userAnswers.set(RemoveSubcontractorVerifyRequestPage(subcontractorId), value)
                            )

                          _ <- sessionRepository.set(updatedAnswers)
                        } yield Redirect(controllers.verify.routes.ReviewUnmatchedSubcontractorsController.onPageLoad())
                      }
                  )
              }
              .getOrElse {
                logger.error(
                  "[RemoveSubcontractorVerifyRequestController][onSubmit] - " +
                    s"subcontractor not found in CurrentVerificationBatchResponsePage, subcontractorId=$subcontractorId"
                )
                Future.successful(recoveryRedirect)
              }

          case None =>
            logger.error(
              "[RemoveSubcontractorVerifyRequestController][onSubmit] - CurrentVerificationBatchResponsePage missing from userAnswers"
            )
            Future.successful(recoveryRedirect)
        }

      result.recover { case ex =>
        logger.error(
          s"[RemoveSubcontractorVerifyRequestController][onSubmit] " +
            s"Failed to remove verification for subcontractorId=$subcontractorId",
          ex
        )

        recoveryRedirect
      }
    }
}
