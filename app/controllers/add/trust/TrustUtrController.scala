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
import forms.add.trust.TrustUtrFormProvider
import models.requests.DataRequest
import models.{AmendMode, FinalValidationMode, Mode}
import navigation.Navigator
import pages.add.trust.{TrustUtrPage, TrustUtrYesNoPage}
import pages.finalvalidation.FinalValidationBaseUtrPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{SubcontractorService, YesOrNoPageGuardService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.add.trust.TrustUtrView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class TrustUtrController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: TrustUtrFormProvider,
  subcontractorService: SubcontractorService,
  redirectVerifiedSubcontractor: RedirectVerifiedSubcontractorAction,
  val controllerComponents: MessagesControllerComponents,
  yesOrNoPageGuardService: YesOrNoPageGuardService,
  view: TrustUtrView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  val form = formProvider()

  private def saveAndContinue(mode: Mode, value: String)(implicit request: DataRequest[?]) =
    for {
      updatedAnswers <-
        Future.fromTry(request.userAnswers.set(TrustUtrPage, value))
      _              <- sessionRepository.set(updatedAnswers)
    } yield Redirect(
      navigator.nextPage(TrustUtrPage, mode, updatedAnswers)
    )

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor) { implicit request =>
      val yesOrNoPage       = TrustUtrYesNoPage
      val yesOrNoPageOption = request.userAnswers.get(TrustUtrYesNoPage)

      SubcontractorNameDisplayHelper
        .getTrustDisplayName(request.userAnswers, mode)
        .map { trustName =>
          val preparedForm = request.userAnswers.get(TrustUtrPage) match {
            case None        => form
            case Some(value) => form.fill(value)
          }
          val result       = Ok(view(preparedForm, mode, trustName))
          yesOrNoPageGuardService.yesOrNoPageRoute(result, yesOrNoPageOption, yesOrNoPage, mode)
        }
        .getOrElse {
          logger.error(s"[TrustUtrController][onPageLoad] - TrustNamePage missing from userAnswers")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }

  def onSubmit(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor).async { implicit request =>
      SubcontractorNameDisplayHelper
        .getTrustDisplayName(request.userAnswers, mode)
        .map { trustName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, trustName))),
              value =>
                val prevValue = request.userAnswers.get(TrustUtrPage)
                val baseValue = request.userAnswers.get(FinalValidationBaseUtrPage)

                mode match {
                  case AmendMode if prevValue.contains(value)                                        =>
                    saveAndContinue(mode, value)
                  case FinalValidationMode if prevValue.contains(value) || baseValue.contains(value) =>
                    saveAndContinue(mode, value)
                  case _                                                                             =>
                    subcontractorService.isDuplicateUTR(request.userAnswers, value).flatMap {
                      case true  =>
                        val errorForm = form
                          .fill(value)
                          .withError(
                            key = "value",
                            message = "trustUtr.error.duplicate"
                          )
                        Future.successful(BadRequest(view(errorForm, mode, trustName)))
                      case false =>
                        saveAndContinue(mode, value)
                    }
                }
            )
        }
        .getOrElse {
          logger.error(s"[TrustUtrController][onSubmit] - TrustNamePage missing from userAnswers")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
    }
}
