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

package controllers.add.company

import controllers.actions.*
import forms.add.company.CompanyPhoneNumberFormProvider
import models.{FinalValidationMode, Mode}
import models.contact.ContactMethodOptions
import navigation.Navigator
import pages.add.company.{CompanyContactMethodOptionsPage, CompanyPhoneNumberPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.company.CompanyPhoneNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CompanyPhoneNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: CompanyPhoneNumberFormProvider,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: CompanyPhoneNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val contactOption = request.userAnswers.get(CompanyContactMethodOptionsPage)
      val companyName   = subcontractorNameExtractor
        .getCompanyName(request.userAnswers, mode)

      val phoneIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Phone))

      (companyName, phoneIsAvailable) match {
        case (Some(companyName), true) =>
          val preparedForm = request.userAnswers.get(CompanyPhoneNumberPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, companyName))

        case (Some(_), false) =>
          Redirect(controllers.add.company.routes.AddCompanyContactMethodsYesNoController.onPageLoad(mode))
        case _                =>
          logger.error("[CompanyPhoneNumberController][onPageLoad] - CompanyNamePage missing from userAnswers")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>

      val contactOption = request.userAnswers.get(CompanyContactMethodOptionsPage)
      val companyName   = subcontractorNameExtractor.getCompanyName(request.userAnswers, mode)

      val phoneIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Phone))

      (for {
        companyName <- companyName
        if phoneIsAvailable
      } yield form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, companyName))),
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(CompanyPhoneNumberPage, value))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(CompanyPhoneNumberPage, mode, updatedAnswers))
        ))
        .getOrElse {
          logger.error(
            "[CompanyPhoneNumberController][onSubmit] - CompanyNamePage missing or phone contact method not selected"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
  }
}
