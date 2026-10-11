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

import config.FrontendAppConfig
import controllers.actions.*
import models.TypeOfSubcontractor
import models.TypeOfSubcontractor.*
import models.requests.DataRequest
import pages.add.CheckYourAnswersSubmittedPage
import pages.add.company.CompanyNamePage
import pages.add.partnership.PartnershipNamePage
import pages.add.trust.TrustNamePage
import play.api.Logging
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import queries.CisIdQuery
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.SubcontractorAddedView

import javax.inject.Inject
import scala.concurrent.Future

class SubcontractorAddedController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents,
  view: SubcontractorAddedView,
  appConfig: FrontendAppConfig
) extends FrontendBaseController
    with I18nSupport
    with Logging {

  def individualSubcontractorAdded: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      subcontractorAdded(TypeOfSubcontractor.Individualorsoletrader)
    }

  def companySubcontractorAdded: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      subcontractorAdded(TypeOfSubcontractor.Limitedcompany)
    }

  def partnershipSubcontractorAdded: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      subcontractorAdded(TypeOfSubcontractor.Partnership)
    }

  def trustSubcontractorAdded: Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      subcontractorAdded(TypeOfSubcontractor.Trust)
    }

  private def subcontractorAdded(
    subcontractorType: TypeOfSubcontractor
  )(implicit request: DataRequest[AnyContent]): Future[Result] = {

    def recoveryRedirect =
      Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

    if (request.userAnswers.get(CheckYourAnswersSubmittedPage).contains(true)) {

      val (name, subcontractorTypeTitle) = subcontractorType match {
        case Individualorsoletrader =>
          (
            subcontractorNameExtractor.getSubcontractorName(request.userAnswers),
            Messages("subcontractorAdded.individual")
          )

        case Limitedcompany =>
          (
            request.userAnswers.get(CompanyNamePage),
            Messages("subcontractorAdded.company")
          )

        case Partnership =>
          (
            request.userAnswers.get(PartnershipNamePage),
            Messages("subcontractorAdded.partnership")
          )

        case Trust =>
          (
            request.userAnswers.get(TrustNamePage),
            Messages("subcontractorAdded.trust")
          )
      }

      val cisId = request.userAnswers.get(CisIdQuery)

      (name, cisId) match {
        case (Some(name), Some(cisId)) =>
          val manageSubcontractorsUrl =
            s"${appConfig.manageSubcontractorsUrl}/$cisId"

          Future.successful(
            Ok(view(name, subcontractorTypeTitle, manageSubcontractorsUrl))
          )

        case _ =>
          logger.error("[SubcontractorAddedController][subcontractorAdded] - name or CIS ID missing from userAnswers")
          Future.successful(recoveryRedirect)
      }

    } else {
      logger.error("[SubcontractorAddedController][subcontractorAdded] - CheckYourAnswersSubmitted not true")
      Future.successful(recoveryRedirect)
    }
  }
}
