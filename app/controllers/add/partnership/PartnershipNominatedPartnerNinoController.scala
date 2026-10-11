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

package controllers.add.partnership

import controllers.actions.*
import controllers.helpers.SubcontractorNameDisplayHelper
import forms.add.partnership.PartnershipNominatedPartnerNinoFormProvider
import models.Mode
import navigation.Navigator
import pages.add.partnership.{PartnershipNominatedPartnerNinoPage, PartnershipNominatedPartnerNinoYesNoPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.YesOrNoPageGuardService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.add.partnership.PartnershipNominatedPartnerNinoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PartnershipNominatedPartnerNinoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PartnershipNominatedPartnerNinoFormProvider,
  yesOrNoPageGuardService: YesOrNoPageGuardService,
  val controllerComponents: MessagesControllerComponents,
  view: PartnershipNominatedPartnerNinoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form = formProvider()

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>
      val yesOrNoPage       = PartnershipNominatedPartnerNinoYesNoPage
      val yesOrNoPageOption = request.userAnswers.get(PartnershipNominatedPartnerNinoYesNoPage)

      SubcontractorNameDisplayHelper
        .getPartnerDisplayName(request.userAnswers, mode)
        .map { nominatedPartnerName =>
          val preparedForm =
            request.userAnswers.get(PartnershipNominatedPartnerNinoPage).fold(form)(form.fill)
          val result       = Ok(view(preparedForm, mode, nominatedPartnerName))
          yesOrNoPageGuardService.yesOrNoPageRoute(result, yesOrNoPageOption, yesOrNoPage, mode)
        }
        .getOrElse {
          logger.error(
            s"[PartnershipNominatedPartnerNinoController][onPageLoad] - partner name missing from userAnswers"
          )
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }

  def onSubmit(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      SubcontractorNameDisplayHelper
        .getPartnerDisplayName(request.userAnswers, mode)
        .map { nominatedPartnerName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, nominatedPartnerName))),
              value =>
                for {
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(PartnershipNominatedPartnerNinoPage, value))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(PartnershipNominatedPartnerNinoPage, mode, updatedAnswers))
            )
        }
        .getOrElse {
          logger.error(
            s"[PartnershipNominatedPartnerNinoController][onSubmit] - partner name missing from userAnswers"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
    }
}
