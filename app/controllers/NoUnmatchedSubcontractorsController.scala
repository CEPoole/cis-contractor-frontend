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

package controllers

import config.FrontendAppConfig
import controllers.actions._
import javax.inject.Inject
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import play.utils.UriEncoding
import queries.CisIdQuery
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.NoUnmatchedSubcontractorsView

class NoUnmatchedSubcontractorsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: NoUnmatchedSubcontractorsView
)(implicit appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    request.userAnswers.get(CisIdQuery) match {
      case Some(cisId) =>
        val manageSubcontractorsUrl =
          s"${appConfig.manageSubcontractorsUrl}/${UriEncoding.encodePathSegment(cisId, "UTF-8")}"
        Ok(view(manageSubcontractorsUrl))
      case None        =>
        logger.error("[NoUnmatchedSubcontractorsController][onPageLoad] - CIS ID missing from userAnswers")
        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
    }
  }
}
