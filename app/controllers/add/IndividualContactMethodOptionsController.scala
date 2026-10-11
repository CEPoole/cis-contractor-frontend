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
import forms.add.IndividualContactMethodOptionsFormProvider
import models.Mode
import models.requests.DataRequest
import navigation.Navigator
import pages.add.{AddIndividualContactMethodsYesNoPage, IndividualContactMethodOptionsPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.YesOrNoPageGuardService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.IndividualContactMethodOptionsView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class IndividualContactMethodOptionsController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: IndividualContactMethodOptionsFormProvider,
  val controllerComponents: MessagesControllerComponents,
  yesOrNoPageGuardService: YesOrNoPageGuardService,
  view: IndividualContactMethodOptionsView,
  subcontractorNameExtractor: SubcontractorNameExtractor
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  private def recoveryRedirect = Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  private def preparedForm(implicit request: DataRequest[?]) =
    request.userAnswers.get(IndividualContactMethodOptionsPage).fold(form)(form.fill)

  def onPageLoad(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>

    val yesOrNoPage       = AddIndividualContactMethodsYesNoPage
    val yesOrNoPageOption = request.userAnswers.get(AddIndividualContactMethodsYesNoPage)

    subcontractorNameExtractor
      .getSubcontractorName(request.userAnswers, mode)
      .fold {
        logger.error(
          "[IndividualContactMethodOptionsController][onPageLoad] - subcontractor name missing from userAnswers"
        )
        recoveryRedirect
      } { subcontractorName =>
        val result = Ok(view(preparedForm, mode, subcontractorName))

        yesOrNoPageGuardService.yesOrNoPageRoute(result, yesOrNoPageOption, yesOrNoPage, mode)
      }
  }

  def onSubmit(mode: Mode): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      subcontractorNameExtractor
        .getSubcontractorName(request.userAnswers, mode)
        .fold {
          logger.error(
            "[IndividualContactMethodOptionsController][onSubmit] - subcontractor name missing from userAnswers"
          )
          Future.successful(recoveryRedirect)
        } { subcontractorName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, subcontractorName))),
              value =>
                for {
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(IndividualContactMethodOptionsPage, value))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(IndividualContactMethodOptionsPage, mode, updatedAnswers))
            )
        }
  }
}
