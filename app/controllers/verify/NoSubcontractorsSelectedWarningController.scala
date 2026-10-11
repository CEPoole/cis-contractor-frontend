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

package controllers.verify

import config.FrontendAppConfig
import controllers.actions.*
import models.{CheckMode, Mode, NormalMode}
import pages.verify.RebuildVerificationFromWarningPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.CisIdQuery
import repositories.SessionRepository
import utils.SubcontractorCleanup
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.verify.NoSubcontractorsSelectedWarningView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class NoSubcontractorsSelectedWarningController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  sessionRepository: SessionRepository,
  val controllerComponents: MessagesControllerComponents,
  view: NoSubcontractorsSelectedWarningView,
  appConfig: FrontendAppConfig
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(): Action[AnyContent] =
    onPageLoadForMode(NormalMode, setRebuildFlag = false)

  def onPageLoadCheckMode(): Action[AnyContent] =
    onPageLoadForMode(CheckMode, setRebuildFlag = true)

  private def onPageLoadForMode(mode: Mode, setRebuildFlag: Boolean): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      request.userAnswers.get(CisIdQuery) match {
        case Some(_) =>
          val selectSubcontractorsUrl =
            controllers.verify.routes.SelectSubcontractorController.onPageLoad(mode).url

          val cancelUrl =
            controllers.verify.routes.NoSubcontractorsSelectedWarningController.onCancel().url

          if (setRebuildFlag) {
            for {
              updatedAnswers <- Future.fromTry(
                                  request.userAnswers.set(RebuildVerificationFromWarningPage, true)
                                )
              _              <- sessionRepository.set(updatedAnswers)
            } yield Ok(view(cancelUrl, selectSubcontractorsUrl))
          } else {
            Future.successful(Ok(view(cancelUrl, selectSubcontractorsUrl)))
          }

        case None =>
          logger.error(
            "[NoSubcontractorsSelectedWarningController][onPageLoadForMode] - CisIdQuery missing from userAnswers"
          )
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      }
    }

  def onCancel(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      request.userAnswers.get(CisIdQuery) match {
        case Some(cisId) =>
          SubcontractorCleanup
            .removeVerifyJourney(request.userAnswers)
            .fold(
              _ =>
                Future.successful {
                  logger.error(
                    "[NoSubcontractorsSelectedWarningController][onCancel] - failed to remove verify journey"
                  )
                  Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
                },
              updatedAnswers =>
                sessionRepository.set(updatedAnswers).map { _ =>
                  Redirect(s"${appConfig.manageSubcontractorsUrl}/$cisId")
                }
            )

        case None =>
          logger.error(
            "[NoSubcontractorsSelectedWarningController][onCancel] - CisIdQuery missing from userAnswers"
          )
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )
      }
    }

}
