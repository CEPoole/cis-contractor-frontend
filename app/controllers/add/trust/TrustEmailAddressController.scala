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
import forms.add.trust.TrustEmailAddressFormProvider
import models.{FinalValidationMode, Mode}
import models.contact.ContactMethodOptions
import navigation.Navigator
import pages.add.trust.{TrustContactMethodOptionsPage, TrustEmailAddressPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.add.trust.TrustEmailAddressView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TrustEmailAddressController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: TrustEmailAddressFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: TrustEmailAddressView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val contactOption = request.userAnswers.get(TrustContactMethodOptionsPage)
      val trustName     = SubcontractorNameDisplayHelper.getTrustDisplayName(request.userAnswers, mode)

      val emailIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Email))

      (trustName, emailIsAvailable) match {
        case (Some(trustName), true) =>
          val preparedForm = request.userAnswers.get(TrustEmailAddressPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, trustName))

        case (Some(_), false) =>
          Redirect(controllers.add.trust.routes.AddTrustContactMethodsYesNoController.onPageLoad(mode))
        case _                =>
          logger.error(s"[TrustEmailAddressController][onPageLoad] - trust name missing from userAnswers, mode=$mode")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>

      val contactOption = request.userAnswers.get(TrustContactMethodOptionsPage)
      val trustName     = SubcontractorNameDisplayHelper.getTrustDisplayName(request.userAnswers, mode)

      val emailIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Email))

      (for {
        trustName <- trustName
        if emailIsAvailable
      } yield form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, trustName))),
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(TrustEmailAddressPage, value))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(TrustEmailAddressPage, mode, updatedAnswers))
        ))
        .getOrElse {
          logger.error(s"[TrustEmailAddressController][onSubmit] - trust name or email option missing, mode=$mode")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
  }
}
