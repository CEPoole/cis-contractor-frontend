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

package controllers.contractordetails

import controllers.actions.*
import forms.contractordetails.RemoveDetailYesNoFormProvider
import pages.contractordetails.RemoveDetailYesNoPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.contractordetails.RemoveDetailYesNoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class RemoveDetailYesNoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: RemoveDetailYesNoFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: RemoveDetailYesNoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val validDetails = Set("email-address", "scheme-name")

  private def withValidDetail(contractorDetail: String)(action: => Future[Result]): Future[Result] =
    if (!validDetails.contains(contractorDetail)) {
      logger.error(s"[RemoveDetailYesNoController][withValidDetail] - invalid contractorDetail=$contractorDetail")
      Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
    } else {
      action
    }

  def onPageLoad(contractorDetail: String): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      withValidDetail(contractorDetail) {
        val form         = formProvider(contractorDetail)
        val preparedForm = request.userAnswers.get(RemoveDetailYesNoPage(contractorDetail)).fold(form)(form.fill)
        Future.successful(Ok(view(contractorDetail, preparedForm)))
      }
    }

  def onSubmit(contractorDetail: String): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      withValidDetail(contractorDetail) {
        formProvider(contractorDetail)
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(view(contractorDetail, formWithErrors))),
            value =>
              for {
                updatedAnswers <-
                  Future.fromTry(request.userAnswers.set(RemoveDetailYesNoPage(contractorDetail), value))
                _              <- sessionRepository.set(updatedAnswers)
              } yield Redirect(
                controllers.contractordetails.routes.ContractorDetailsCheckAnswersController.onPageLoad().url
              )
          )
      }
    }
}
