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

package controllers.add

import controllers.actions.*
import controllers.routes
import forms.add.IndividualEmailAddressFormProvider
import models.{FinalValidationMode, Mode}
import models.contact.ContactMethodOptions
import navigation.Navigator
import pages.add.{IndividualContactMethodOptionsPage, IndividualEmailAddressPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Results.Redirect
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.IndividualEmailAddressView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class IndividualEmailAddressController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: IndividualEmailAddressFormProvider,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: IndividualEmailAddressView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val contactOption     = request.userAnswers.get(IndividualContactMethodOptionsPage)
      val subcontractorName = subcontractorNameExtractor.getSubcontractorName(request.userAnswers, mode)

      val emailIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Email))

      (subcontractorName, emailIsAvailable) match {
        case (Some(subcontractorName), true) =>
          val preparedForm = request.userAnswers.get(IndividualEmailAddressPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, subcontractorName))

        case (Some(_), false) =>
          Redirect(controllers.add.routes.IndividualContactMethodOptionsController.onPageLoad(mode))
        case _                =>
          logger.error("[IndividualEmailAddressController][onPageLoad] - subcontractor name missing from userAnswers")
          Redirect(routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>

      val contactOption     = request.userAnswers.get(IndividualContactMethodOptionsPage)
      val subcontractorName = subcontractorNameExtractor.getSubcontractorName(request.userAnswers, mode)

      val emailIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Email))

      (for {
        subcontractorName <- subcontractorName
        if emailIsAvailable
      } yield form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, subcontractorName))),
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(IndividualEmailAddressPage, value))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(IndividualEmailAddressPage, mode, updatedAnswers))
        ))
        .getOrElse {
          logger.error(
            "[IndividualEmailAddressController][onSubmit] - subcontractor name missing or email not selected"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
  }
}
