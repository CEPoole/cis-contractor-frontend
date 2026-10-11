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

package controllers.amend.partnership

import config.FrontendAppConfig
import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import controllers.helpers.SubcontractorNameDisplayHelper
import controllers.routes
import models.AmendMode
import models.amend.AmendJourneyType
import pages.amend.{AmendCheckYourAnswersSubmittedPage, AmendJourneyTypePage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.{CisIdQuery, OriginalPartnershipAnswersQuery}
import repositories.SessionRepository
import services.VerificationService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.amend.AmendConfirmationLinks
import viewmodels.checkAnswers.amend.partnership.AmendPartnershipConfirmationViewModel
import views.html.amend.AmendConfirmationView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AmendPartnershipConfirmationController @Inject() (
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
          s"[AmendPartnershipConfirmationController][onPageLoad] " +
            "Accessed confirmation page without prior submission"
        )
        Future.successful(Redirect(routes.JourneyRecoveryController.onPageLoad()))
      } else {

        ua.get(OriginalPartnershipAnswersQuery) match {

          case None =>
            logger.error("[AmendPartnershipConfirmationController] Missing OriginalPartnershipAnswersQuery")
            Future.successful(recoveryRedirect)

          case Some(originalPartnershipAnswers) =>
            ua.get(CisIdQuery) match {

              case None =>
                logger.error("[AmendPartnershipConfirmationController] Missing CisIdQuery")
                Future.successful(recoveryRedirect)

              case Some(cisId) =>
                ua.get(AmendJourneyTypePage) match {

                  case Some(journeyType) =>
                    val tableRows =
                      AmendPartnershipConfirmationViewModel.rows(
                        originalPartnershipAnswers,
                        ua
                      )

                    val partnershipName =
                      SubcontractorNameDisplayHelper.partnershipDisplayName(ua, AmendMode)

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
                            partnershipName,
                            confirmationLink
                          )
                        )
                      }
                      .recover { case exception =>
                        logger.error(
                          "[AmendPartnershipConfirmationController.onPageLoad] " +
                            "Failed to persist confirmation session data",
                          exception
                        )

                        recoveryRedirect
                      }

                  case None =>
                    logger.error(
                      "[AmendPartnershipConfirmationController] Missing AmendJourneyTypePage"
                    )

                    Future.successful(recoveryRedirect)
                }
            }
        }
      }
    }

}
