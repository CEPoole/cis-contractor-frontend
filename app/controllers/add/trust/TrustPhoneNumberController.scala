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
import forms.add.trust.TrustPhoneNumberFormProvider
import models.{FinalValidationMode, Mode}
import models.contact.ContactMethodOptions
import navigation.Navigator
import pages.add.trust.{TrustContactMethodOptionsPage, TrustPhoneNumberPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.add.trust.TrustPhoneNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TrustPhoneNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: TrustPhoneNumberFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: TrustPhoneNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val contactOption = request.userAnswers.get(TrustContactMethodOptionsPage)
      val trustName     = SubcontractorNameDisplayHelper.getTrustDisplayName(request.userAnswers, mode)

      val phoneIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Phone))

      (trustName, phoneIsAvailable) match {
        case (Some(trustName), true) =>
          val preparedForm = request.userAnswers.get(TrustPhoneNumberPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, trustName))

        case (Some(_), false) =>
          Redirect(controllers.add.trust.routes.AddTrustContactMethodsYesNoController.onPageLoad(mode))
        case _                =>
          logger.error(s"[TrustPhoneNumberController][onPageLoad] - trust name missing from userAnswers, mode=$mode")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>

      val contactOption = request.userAnswers.get(TrustContactMethodOptionsPage)
      val trustName     = SubcontractorNameDisplayHelper.getTrustDisplayName(request.userAnswers, mode)

      val phoneIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Phone))

      (for {
        trustName <- trustName
        if phoneIsAvailable
      } yield form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, trustName))),
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(TrustPhoneNumberPage, value))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(TrustPhoneNumberPage, mode, updatedAnswers))
        ))
        .getOrElse {
          logger.error(s"[TrustPhoneNumberController][onSubmit] - trust name or phone option missing, mode=$mode")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
  }
}
