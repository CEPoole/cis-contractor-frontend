/*
 * Copyright 2025 HM Revenue & Customs
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
import forms.add.WorksReferenceNumberYesNoFormProvider
import models.Mode
import models.requests.DataRequest
import navigation.Navigator
import pages.add.WorksReferenceNumberYesNoPage
import play.api.data.Form
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.WorksReferenceNumberYesNoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class WorksReferenceNumberYesNoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: WorksReferenceNumberYesNoFormProvider,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: WorksReferenceNumberYesNoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form: Form[Boolean] = formProvider()

  private def recoveryRedirect =
    Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  private def preparedForm(implicit request: DataRequest[?]) =
    request.userAnswers.get(WorksReferenceNumberYesNoPage).fold(form)(form.fill)

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>
      subcontractorNameExtractor
        .getSubcontractorName(request.userAnswers, mode)
        .fold {
          logger.error("[WorksReferenceNumberYesNoController][onPageLoad] - subcontractor name missing or invalid")
          recoveryRedirect
        } { subcontractorName =>
          Ok(view(preparedForm, mode, subcontractorName))
        }
    }

  def onSubmit(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      subcontractorNameExtractor
        .getSubcontractorName(request.userAnswers, mode)
        .fold {
          logger.error("[WorksReferenceNumberYesNoController][onSubmit] - subcontractor name missing or invalid")
          Future.successful(recoveryRedirect)
        } { subcontractorName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, subcontractorName))),
              value =>
                for {
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(WorksReferenceNumberYesNoPage, value))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(WorksReferenceNumberYesNoPage, mode, updatedAnswers))
            )
        }
    }
}
