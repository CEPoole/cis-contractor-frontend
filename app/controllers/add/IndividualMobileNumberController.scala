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
import forms.add.IndividualMobileNumberFormProvider
import models.{FinalValidationMode, Mode}
import models.contact.ContactMethodOptions
import navigation.Navigator
import pages.add.{IndividualContactMethodOptionsPage, IndividualMobileNumberPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.IndividualMobileNumberView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class IndividualMobileNumberController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: IndividualMobileNumberFormProvider,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: IndividualMobileNumberView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val contactOption     = request.userAnswers.get(IndividualContactMethodOptionsPage)
      val subcontractorName = subcontractorNameExtractor.getSubcontractorName(request.userAnswers, mode)

      val mobileIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Mobile))

      (subcontractorName, mobileIsAvailable) match {
        case (Some(subcontractorName), true) =>
          val preparedForm = request.userAnswers.get(IndividualMobileNumberPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          Ok(view(preparedForm, mode, subcontractorName))

        case (Some(_), false) =>
          Redirect(controllers.add.routes.IndividualContactMethodOptionsController.onPageLoad(mode))
        case _                =>
          logger.error("[IndividualMobileNumberController][onPageLoad] - subcontractor name missing from userAnswers")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>

      val contactOption     = request.userAnswers.get(IndividualContactMethodOptionsPage)
      val subcontractorName = subcontractorNameExtractor.getSubcontractorName(request.userAnswers, mode)

      val mobileIsAvailable =
        mode == FinalValidationMode ||
          contactOption.exists(_.contains(ContactMethodOptions.Mobile))

      (for {
        subcontractorName <- subcontractorName
        if mobileIsAvailable
      } yield form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, subcontractorName))),
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(IndividualMobileNumberPage, value))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(IndividualMobileNumberPage, mode, updatedAnswers))
        ))
        .getOrElse {
          logger.error(
            "[IndividualMobileNumberController][onSubmit] - subcontractor name missing or mobile not selected"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
  }
}
