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
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import queries.CisIdQuery
import models.response.GetNewestVerificationBatchResponse
import pages.verify.NewestVerificationBatchResponsePage
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.verify.InactiveSchemeWarningView

import javax.inject.Inject
import java.time.LocalDateTime

class InactiveSchemeWarningController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: InactiveSchemeWarningView
)(implicit appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    request.userAnswers.get(NewestVerificationBatchResponsePage) match {

      case Some(response) if isSchemeInactive(response) =>
        request.userAnswers.get(CisIdQuery) match {

          case Some(cisId) =>
            val manageSubcontractorsUrl =
              s"${appConfig.manageSubcontractorsUrl}/$cisId"

            Ok(view(manageSubcontractorsUrl))

          case None =>
            logger.error(
              "[InactiveSchemeWarningController][onPageLoad] - CisIdQuery missing from userAnswers"
            )
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }

      case _ =>
        logger.error(
          "[InactiveSchemeWarningController][onPageLoad] - NewestVerificationBatchResponsePage missing or scheme not inactive"
        )
        Redirect(
          controllers.routes.JourneyRecoveryController.onPageLoad()
        )
    }
  }

  private def isSchemeInactive(
    response: models.response.GetNewestVerificationBatchResponse
  ): Boolean =
    response.monthlyReturn.exists(
      _.decNoMoreSubPayments.contains("Y")
    ) &&
      response.monthlyReturnSubmission
        .flatMap(_.submissionRequestDate)
        .exists { submissionDate =>
          LocalDateTime
            .now()
            .isBefore(
              submissionDate.plusMonths(6)
            )
        }
}
