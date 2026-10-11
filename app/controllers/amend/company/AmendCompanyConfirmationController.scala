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

package controllers.amend.company

import config.FrontendAppConfig
import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import controllers.routes
import models.amend.AmendJourneyType
import pages.add.company.CompanyNamePage
import pages.amend.{AmendCheckYourAnswersSubmittedPage, AmendJourneyTypePage}
import play.api.Logging
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.{CisIdQuery, OriginalCompanyAnswersQuery}
import repositories.SessionRepository
import services.VerificationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.amend.AmendConfirmationLinks
import viewmodels.amend.company.CompanyAmendConfirmationViewModel
import views.html.amend.AmendConfirmationView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AmendCompanyConfirmationController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  verificationService: VerificationService,
  sessionRepository: SessionRepository,
  view: AmendConfirmationView,
  appConfig: FrontendAppConfig
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      val recoveryRedirect =
        Redirect(routes.JourneyRecoveryController.onPageLoad())

      val ua = request.userAnswers

      if (!ua.get(AmendCheckYourAnswersSubmittedPage).contains(true)) {
        logger.error(
          "[AmendCompanyConfirmationController] Accessed without prior CYA submission"
        )
        Future.successful(recoveryRedirect)
      } else {
        ua.get(OriginalCompanyAnswersQuery) match {

          case None =>
            logger.error(
              "[AmendCompanyConfirmationController] Missing OriginalCompanyAnswersQuery"
            )
            Future.successful(recoveryRedirect)

          case Some(originalCompanyAnswers) =>
            ua.get(CisIdQuery) match {

              case None =>
                logger.error(
                  "[AmendCompanyConfirmationController] Missing CisIdQuery"
                )
                Future.successful(recoveryRedirect)

              case Some(cisId) =>
                ua.get(AmendJourneyTypePage) match {

                  case Some(journeyType) =>
                    val tableRows =
                      CompanyAmendConfirmationViewModel.rows(
                        originalCompanyAnswers,
                        ua
                      )

                    val companyName =
                      ua.get(CompanyNamePage).getOrElse(Messages("verify.noName"))

                    val confirmationLink =
                      AmendConfirmationLinks.build(
                        journeyType,
                        cisId,
                        appConfig
                      )

                    val persistFinalUserAnswers =
                      journeyType match {

                        case AmendJourneyType.Standard =>
                          sessionRepository
                            .set(ua)
                            .map(_ => ua)

                        case AmendJourneyType.InsufficientInfo | AmendJourneyType.UnmatchedInfo =>
                          verificationService.refreshVerificationBatches(
                            ua
                          )
                      }

                    persistFinalUserAnswers
                      .map { _ =>
                        Ok(
                          view(
                            tableRows,
                            companyName,
                            confirmationLink
                          )
                        )
                      }
                      .recover { case exception =>
                        logger.error(
                          "[AmendCompanyConfirmationController.onPageLoad] " +
                            "Failed to persist confirmation session data",
                          exception
                        )

                        recoveryRedirect
                      }

                  case None =>
                    logger.error(
                      "[AmendCompanyConfirmationController] Missing AmendJourneyTypePage"
                    )

                    Future.successful(recoveryRedirect)
                }
            }
        }
      }
    }
}
