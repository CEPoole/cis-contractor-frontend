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

package controllers.add.trust

import controllers.actions.*
import controllers.helpers.SubcontractorNameDisplayHelper
import forms.add.trust.TrustUtrYesNoFormProvider
import models.Mode
import navigation.Navigator
import pages.add.trust.TrustUtrYesNoPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.add.trust.TrustUtrYesNoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TrustUtrYesNoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: TrustUtrYesNoFormProvider,
  redirectVerifiedSubcontractor: RedirectVerifiedSubcontractorAction,
  val controllerComponents: MessagesControllerComponents,
  view: TrustUtrYesNoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor) { implicit request =>
      SubcontractorNameDisplayHelper
        .getTrustDisplayName(request.userAnswers, mode)
        .map { trustName =>
          val preparedForm = request.userAnswers.get(TrustUtrYesNoPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, trustName))
        }
        .getOrElse {
          logger.error(s"[TrustUtrYesNoController][onPageLoad] - TrustNamePage missing from userAnswers")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }

  def onSubmit(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor).async { implicit request =>
      SubcontractorNameDisplayHelper
        .getTrustDisplayName(request.userAnswers, mode)
        .map { trustName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, trustName))),
              value =>
                for {
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(TrustUtrYesNoPage, value))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(TrustUtrYesNoPage, mode, updatedAnswers))
            )
        }
        .getOrElse {
          logger.error(s"[TrustUtrYesNoController][onSubmit] - TrustNamePage missing from userAnswers")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
    }
}
