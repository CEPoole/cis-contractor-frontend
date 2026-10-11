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
import forms.add.company.CompanyMobileNumberFormProvider
import models.{FinalValidationMode, Mode}
import models.contact.ContactMethodOptions
import navigation.Navigator
import pages.add.company.{CompanyContactMethodOptionsPage, CompanyMobileNumberPage}
import play.api.Logging
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.company.CompanyMobileNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CompanyMobileNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: CompanyMobileNumberFormProvider,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: CompanyMobileNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form: Form[String] = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val contactOption = request.userAnswers.get(CompanyContactMethodOptionsPage)
      val companyName   = subcontractorNameExtractor
        .getCompanyName(request.userAnswers, mode)

      val mobileIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Mobile))

      (companyName, mobileIsAvailable) match {
        case (Some(companyName), true) =>
          val preparedForm = request.userAnswers.get(CompanyMobileNumberPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, companyName))

        case (Some(_), false) =>
          Redirect(controllers.add.company.routes.AddCompanyContactMethodsYesNoController.onPageLoad(mode))
        case _                =>
          logger.error("[CompanyMobileNumberController][onPageLoad] - CompanyNamePage missing from userAnswers")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>

      val contactOption = request.userAnswers.get(CompanyContactMethodOptionsPage)
      val companyName   = subcontractorNameExtractor.getCompanyName(request.userAnswers, mode)

      val mobileIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Mobile))

      (for {
        companyName <- companyName
        if mobileIsAvailable
      } yield form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, companyName))),
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(CompanyMobileNumberPage, value))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(CompanyMobileNumberPage, mode, updatedAnswers))
        ))
        .getOrElse {
          logger.error(
            "[CompanyMobileNumberController][onSubmit] - CompanyNamePage missing or mobile contact method not selected"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
  }
}
