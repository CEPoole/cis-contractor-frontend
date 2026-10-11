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
import models.Scheme
import models.requests.{DataRequest, UpdateContractorSchemeParams}
import pages.contractordetails.{AddEmailAddressYesNoPage, AddSchemeNameYesNoPage, ContractorSchemePage, ContractorUtrPage, EnterContractorEmailAddressPage, SchemeNamePage}
import pages.CisIdPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import services.ContractorDetailsService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.checkAnswers.contractordetails.*
import views.html.contractordetails.ContractorDetailsCheckAnswersView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ContractorDetailsCheckAnswersController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  service: ContractorDetailsService,
  val controllerComponents: MessagesControllerComponents,
  view: ContractorDetailsCheckAnswersView
)(implicit ec: ExecutionContext, appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(target: Option[String] = None): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>

      val cisAccountUrl =
        if (!request.isAgent) {
          appConfig.constructionIndustryOrgAccountUrl
        } else {
          request.userAnswers
            .get(CisIdPage)
            .fold(appConfig.constructionIndustryAgentAccountUrl)(cisId =>
              s"${appConfig.constructionIndustryAgentAccountUrl}$cisId"
            )
        }

      request.userAnswers.get(ContractorSchemePage) match {
        case Some(scheme) =>
          if (hasRequiredDetails(request)) {
            val summaryRows = Seq(
              ContractorUtrSummary.row(request.userAnswers),
              AddSchemeNameYesNoSummary.row(request.userAnswers),
              SchemeNameSummary.row(request.userAnswers),
              AddEmailAddressYesNoSummary.row(request.userAnswers),
              EnterContractorEmailAddressSummary.row(request.userAnswers)
            ).flatten

            Ok(
              view(
                scheme.accountsOfficeReference,
                summaryRows,
                cisAccountUrl,
                validTarget(target)
              )
            )
          } else {
            logger.error("[ContractorDetailsCheckAnswersController][onPageLoad] - required details invalid")
            Redirect(
              controllers.routes.JourneyRecoveryController.onPageLoad()
            )
          }

        case None =>
          logger.error("[ContractorDetailsCheckAnswersController][onPageLoad] - scheme missing from userAnswers")
          Redirect(
            controllers.routes.JourneyRecoveryController.onPageLoad()
          )
      }
    }

  def onSubmit(target: Option[String] = None): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      request.userAnswers.get(ContractorSchemePage) match {
        case Some(scheme) if hasRequiredDetails(request) =>
          validTarget(target)
            .map(target => Future.successful(Redirect(targetUrl(target, scheme.instanceId))))
            .getOrElse(updateContractorDetails(scheme))

        case _ =>
          logger.error("[ContractorDetailsCheckAnswersController][onSubmit] - scheme or required details invalid")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      }
    }

  private def updateContractorDetails(
    scheme: Scheme
  )(implicit request: DataRequest[AnyContent]): Future[Result] = {
    val updateRequest = UpdateContractorSchemeParams(
      schemeId = scheme.schemeId,
      instanceId = scheme.instanceId,
      accountsOfficeReference = scheme.accountsOfficeReference,
      taxOfficeNumber = scheme.taxOfficeNumber,
      taxOfficeReference = scheme.taxOfficeReference,
      utr = request.userAnswers.get(ContractorUtrPage),
      name = request.userAnswers.get(SchemeNamePage),
      emailAddress = request.userAnswers.get(EnterContractorEmailAddressPage),
      version = scheme.version,
      displayWelcomePage = scheme.displayWelcomePage,
      prePopCount = scheme.prePopCount,
      prePopSuccessful = scheme.prePopSuccessful
    )

    service
      .updateContractorDetails(updateRequest)
      .map(_ => Redirect(routes.ContractorDetailsUpdatedController.onPageLoad()))
      .recover { case t =>
        logger.error(
          "[ContractorDetailsCheckAnswersController.onSubmit] Failed to update contractor details",
          t
        )
        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
  }

  private def hasRequiredDetails(request: DataRequest[AnyContent]): Boolean = {
    val schemeNameValid = request.userAnswers.get(AddSchemeNameYesNoPage) match {
      case Some(true)  => request.userAnswers.get(SchemeNamePage).isDefined
      case Some(false) => true
      case None        => false
    }

    val emailAddressValid = request.userAnswers.get(AddEmailAddressYesNoPage) match {
      case Some(true)  => request.userAnswers.get(EnterContractorEmailAddressPage).isDefined
      case Some(false) => request.userAnswers.get(EnterContractorEmailAddressPage).isEmpty
      case None        => false
    }

    schemeNameValid && emailAddressValid
  }

  private val ManageYourCisReturnTarget = "manageYourCisReturn"
  private val SubcontractorsTarget      = "subcontractors"

  private def validTarget(target: Option[String]): Option[String] =
    target.filter {
      case ManageYourCisReturnTarget | SubcontractorsTarget => true
      case _                                                => false
    }

  private def targetUrl(target: String, instanceId: String): String =
    target match {
      case ManageYourCisReturnTarget =>
        appConfig.manageCisReturnUrl(instanceId)

      case SubcontractorsTarget =>
        appConfig.manageSubcontractorsLandingUrl(instanceId)
    }
}
