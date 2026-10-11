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

import controllers.actions.*
import forms.unmatched.ProceedSubcontractorVerifyRequestFormProvider
import models.Mode
import navigation.verify.VerifyNavigator
import pages.unmatched.ProceedSubcontractorVerifyRequestPage
import pages.verify.CurrentVerificationBatchResponsePage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.CisIdQuery
import repositories.SessionRepository
import services.VerificationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.unmatched.ProceedSubcontractorVerifyRequestView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ProceedSubcontractorVerifyRequestController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: VerifyNavigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: ProceedSubcontractorVerifyRequestFormProvider,
  verificationBatchService: VerificationService,
  val controllerComponents: MessagesControllerComponents,
  view: ProceedSubcontractorVerifyRequestView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form             = formProvider()
  private def recoveryRedirect =
    Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  def onPageLoad(subcontractorId: Long, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val page =
        ProceedSubcontractorVerifyRequestPage(subcontractorId.toString)

      request.userAnswers
        .get(CurrentVerificationBatchResponsePage)
        .flatMap { batch =>
          batch.subcontractors
            .find(_.subcontractorId == subcontractorId)
            .map { subcontractor =>

              val verification =
                batch.verifications.find(
                  _.subcontractorId.contains(subcontractorId)
                )

              if (verification.exists(_.proceed.exists(_.trim.equalsIgnoreCase("Y")))) {

                Redirect(
                  navigator.nextPage(
                    page,
                    mode,
                    request.userAnswers
                  )
                )

              } else {

                val preparedForm =
                  request.userAnswers
                    .get(page)
                    .fold(form)(form.fill)

                Ok(
                  view(
                    preparedForm,
                    subcontractor.displayName,
                    subcontractorId
                  )
                )
              }
            }
        }
        .getOrElse {
          logger.error(
            "[ProceedSubcontractorVerifyRequestController][onPageLoad] - " +
              s"CurrentVerificationBatchResponsePage missing or subcontractor not in batch, subcontractorId=$subcontractorId"
          )
          recoveryRedirect
        }
    }

  def onSubmit(subcontractorId: Long, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      val result =
        (request.userAnswers.get(CisIdQuery), request.userAnswers.get(CurrentVerificationBatchResponsePage)) match {
          case (Some(cisId), Some(batch)) =>
            batch.subcontractors
              .find(_.subcontractorId == subcontractorId)
              .map { subcontractor =>
                form
                  .bindFromRequest()
                  .fold(
                    formWithErrors =>
                      Future.successful(
                        BadRequest(
                          view(
                            formWithErrors,
                            subcontractor.displayName,
                            subcontractorId
                          )
                        )
                      ),
                    value =>
                      if (value) {
                        for {
                          _                         <-
                            verificationBatchService.proceedUnmatchedVerification(cisId, subcontractorId, batch)
                          updatedAnswers            <-
                            Future.fromTry(
                              request.userAnswers
                                .set(ProceedSubcontractorVerifyRequestPage(subcontractorId.toString), value)
                            )
                          uaWithUpdatedCurrentBatch <-
                            verificationBatchService.getCurrentVerificationBatch(updatedAnswers)
                          uaWithNewestBatch         <-
                            verificationBatchService.refreshNewestVerificationBatch(uaWithUpdatedCurrentBatch)
                          _                         <- sessionRepository.set(uaWithNewestBatch)
                        } yield Redirect(
                          navigator.nextPage(
                            ProceedSubcontractorVerifyRequestPage(subcontractorId.toString),
                            mode,
                            uaWithNewestBatch
                          )
                        )
                      } else {
                        for {
                          updatedAnswers <-
                            Future.fromTry(
                              request.userAnswers
                                .set(ProceedSubcontractorVerifyRequestPage(subcontractorId.toString), value)
                            )
                          _              <- sessionRepository.set(updatedAnswers)
                        } yield Redirect(
                          navigator.nextPage(
                            ProceedSubcontractorVerifyRequestPage(subcontractorId.toString),
                            mode,
                            updatedAnswers
                          )
                        )
                      }
                  )
              }
              .getOrElse {
                logger.error(
                  "[ProceedSubcontractorVerifyRequestController][onSubmit] - " +
                    s"subcontractor not in CurrentVerificationBatchResponsePage, subcontractorId=$subcontractorId"
                )
                Future.successful(recoveryRedirect)
              }

          case _ =>
            logger.error(
              "[ProceedSubcontractorVerifyRequestController][onSubmit] - " +
                "CisIdQuery or CurrentVerificationBatchResponsePage missing from userAnswers"
            )
            Future.successful(recoveryRedirect)
        }

      result.recover { case ex =>
        logger.error(
          s"[ProceedSubcontractorVerifyRequestController][onSubmit] Failed to submit unmatched verification for subcontractorId=$subcontractorId",
          ex
        )

        recoveryRedirect
      }
    }
}
