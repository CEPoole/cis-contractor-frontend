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

package controllers.add.company

import controllers.actions.*
import models.add.company.ValidatedCompany
import pages.add.CheckYourAnswersSubmittedPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{AuditService, SubcontractorService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.checkAnswers.add.TypeOfSubcontractorSummary
import viewmodels.checkAnswers.add.company.*
import viewmodels.govuk.summarylist.*
import views.html.add.company.CompanyCheckYourAnswersView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CompanyCheckYourAnswersController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  subcontractorService: SubcontractorService,
  sessionRepository: SessionRepository,
  auditService: AuditService,
  view: CompanyCheckYourAnswersView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>
      val ua = request.userAnswers

      ValidatedCompany.build(ua) match {
        case Right(_) =>
          val list = SummaryListViewModel(
            rows = Seq(
              TypeOfSubcontractorSummary.row(ua),
              CompanyNameSummary.row(ua),
              CompanyAddressYesNoSummary.row(ua),
              CompanyAddressSummary.row(ua),
              AddCompanyContactMethodsYesNoSummary.row(ua),
              CompanyContactMethodOptionsSummary.row(ua),
              CompanyEmailAddressSummary.row(ua),
              CompanyPhoneNumberSummary.row(ua),
              CompanyMobileNumberSummary.row(ua),
              CompanyUtrYesNoSummary.row(ua),
              CompanyUtrSummary.row(ua),
              CompanyCrnYesNoSummary.row(ua),
              CompanyCrnSummary.row(ua),
              CompanyWorksReferenceYesNoSummary.row(ua),
              CompanyWorksReferenceSummary.row(ua)
            ).flatten
          )

          Ok(view(list))

        case Left(error) =>
          logger.error(s"[CompanyCheckYourAnswersController.onPageLoad] Failed to load the page: $error")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      if (request.userAnswers.get(CheckYourAnswersSubmittedPage).contains(true)) {
        logger.error(
          "[CompanyCheckYourAnswersController][onSubmit] - CheckYourAnswersSubmittedPage is true, resubmission blocked"
        )
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      } else {
        ValidatedCompany.build(request.userAnswers) match {

          case Right(_) =>
            subcontractorService
              .createAndUpdateSubcontractor(request.userAnswers)
              .flatMap { _ =>
                auditService.addSubcontractorEvent(request.userAnswers)
                Future
                  .fromTry(request.userAnswers.set(CheckYourAnswersSubmittedPage, true))
                  .flatMap(updated => sessionRepository.set(updated).map(_ => ()))
                  .map(_ => Redirect(controllers.add.routes.SubcontractorAddedController.companySubcontractorAdded()))
              }
              .recover { case t =>
                logger.error(
                  "[CompanyCheckYourAnswersController.onSubmit] Failed to create/update company subcontractor",
                  t
                )
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              }

          case Left(error) =>
            logger.error(s"[CompanyCheckYourAnswersController.onSubmit] Validation failed: $error")
            Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
    }
}
