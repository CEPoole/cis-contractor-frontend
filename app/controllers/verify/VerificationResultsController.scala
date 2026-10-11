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

import config.FrontendAppConfig
import controllers.actions.*
import models.UserAnswers
import pages.unmatched.RemoveSubcontractorVerifyRequestPage
import pages.verify.LastSubmittedVerificationBatchResponsePage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.verify.VerificationResultsViewModel
import views.html.verify.VerificationResultsView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class VerificationResultsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  requireCisId: CisIdRequiredAction,
  sessionRepository: SessionRepository,
  val controllerComponents: MessagesControllerComponents,
  view: VerificationResultsView
)(implicit ec: ExecutionContext, appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData andThen requireCisId).async {
    implicit request =>
      request.userAnswers.get(LastSubmittedVerificationBatchResponsePage) match {
        case Some(response) =>
          val manageSubcontractorsUrl = s"${appConfig.manageSubcontractorsUrl}/${request.cisId}"
          cleanseSessionPages(request.userAnswers)
            .map { _ =>
              Ok(view(VerificationResultsViewModel.from(response), manageSubcontractorsUrl))
            }
        case None           =>
          logger.error(
            "[VerificationResultsController][onPageLoad] - LastSubmittedVerificationBatchResponsePage missing from userAnswers"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      }
  }

  private def cleanseSessionPages(userAnswers: UserAnswers): Future[UserAnswers] =
    for {
      updatedUserAnswers <- Future.fromTry(userAnswers.remove(RemoveSubcontractorVerifyRequestPage.All))
      _                  <- sessionRepository.set(updatedUserAnswers)
    } yield updatedUserAnswers
}
