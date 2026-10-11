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

import config.FrontendAppConfig
import controllers.actions.*
import pages.CisIdPage
import pages.contractordetails.ContractorSchemePage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.ContractorDetailsService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.contractordetails.ContractorDetailsUpdatedView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ContractorDetailsUpdatedController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  contractorDetailsService: ContractorDetailsService,
  sessionRepository: SessionRepository,
  val controllerComponents: MessagesControllerComponents,
  view: ContractorDetailsUpdatedView
)(implicit
  appConfig: FrontendAppConfig,
  ec: ExecutionContext
) extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      request.userAnswers.get(CisIdPage) match {

        case None =>
          logger.error("[ContractorDetailsUpdatedController][onPageLoad] - CisIdPage missing from userAnswers")
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )

        case Some(cisId) =>
          val cisAccountUrl =
            if (request.isAgent) {
              s"${appConfig.constructionIndustryAgentAccountUrl}$cisId"
            } else {
              appConfig.constructionIndustryOrgAccountUrl
            }

          contractorDetailsService
            .getScheme(cisId)
            .flatMap { latestScheme =>
              Future.fromTry(
                request.userAnswers.set(
                  ContractorSchemePage,
                  latestScheme
                )
              )
            }
            .flatMap { updatedAnswers =>
              sessionRepository
                .set(updatedAnswers)
                .map { _ =>
                  Ok(view(cisAccountUrl))
                }
            }
            .recover { case error =>
              logger.error(
                "[ContractorDetailsUpdatedController] Failed to refresh contractor details",
                error
              )

              Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
            }
      }
    }
}
