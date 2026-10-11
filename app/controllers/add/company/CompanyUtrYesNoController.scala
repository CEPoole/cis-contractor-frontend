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
import forms.add.company.CompanyUtrYesNoFormProvider
import models.Mode
import navigation.Navigator
import pages.add.company.CompanyUtrYesNoPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.company.CompanyUtrYesNoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CompanyUtrYesNoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: CompanyUtrYesNoFormProvider,
  redirectVerifiedSubcontractor: RedirectVerifiedSubcontractorAction,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: CompanyUtrYesNoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor) { implicit request =>
      subcontractorNameExtractor
        .getCompanyName(request.userAnswers, mode)
        .map { companyName =>
          val preparedForm = request.userAnswers.get(CompanyUtrYesNoPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }

          Ok(view(preparedForm, mode, companyName))
        }
        .getOrElse {
          logger.error("[CompanyUtrYesNoController][onPageLoad] - CompanyNamePage missing from userAnswers")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }

  def onSubmit(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor).async { implicit request =>
      subcontractorNameExtractor
        .getCompanyName(request.userAnswers, mode)
        .map { companyName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, companyName))),
              value =>
                for {
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(CompanyUtrYesNoPage, value))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(CompanyUtrYesNoPage, mode, updatedAnswers))
            )
        }
        .getOrElse {
          logger.error("[CompanyUtrYesNoController][onSubmit] - CompanyNamePage missing from userAnswers")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
    }
}
