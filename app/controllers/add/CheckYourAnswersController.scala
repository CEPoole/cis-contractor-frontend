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

import com.google.inject.Inject
import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import models.add.ValidatedSubcontractor
import pages.add.CheckYourAnswersSubmittedPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{AuditService, SubcontractorService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.checkAnswers.add.*
import viewmodels.govuk.summarylist.*
import views.html.add.CheckYourAnswersView

import scala.concurrent.{ExecutionContext, Future}

class CheckYourAnswersController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  subcontractorService: SubcontractorService,
  sessionRepository: SessionRepository,
  auditService: AuditService,
  view: CheckYourAnswersView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(): Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val ua = request.userAnswers
    ValidatedSubcontractor.build(ua) match {
      case Right(_)    =>
        val list = SummaryListViewModel(
          rows = Seq(
            TypeOfSubcontractorSummary.row(ua),
            IndividualNamesOptionsSummary.row(ua),
            SubcontractorNameSummary.row(ua),
            TradingNameOfSubcontractorSummary.row(ua),
            SubAddressYesNoSummary.row(ua),
            AddressOfSubcontractorSummary.row(ua),
            AddIndividualContactMethodsYesNoSummary.row(ua),
            IndividualContactMethodOptionsSummary.row(ua),
            IndividualEmailAddressSummary.row(ua),
            IndividualPhoneNumberSummary.row(ua),
            IndividualMobileNumberSummary.row(ua),
            UniqueTaxpayerReferenceYesNoSummary.row(ua),
            SubcontractorsUniqueTaxpayerReferenceSummary.row(ua),
            NationalInsuranceNumberYesNoSummary.row(ua),
            SubNationalInsuranceNumberSummary.row(ua),
            WorksReferenceNumberYesNoSummary.row(ua),
            WorksReferenceNumberSummary.row(ua)
          ).flatten
        )

        Ok(view(list))
      case Left(error) =>
        logger.error(s"[CheckYourAnswersController.onPageLoad] Failed to load the page: $error")
        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
    }
  }

  def onSubmit(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      if (request.userAnswers.get(CheckYourAnswersSubmittedPage).contains(true)) {
        logger.error(
          "[CheckYourAnswersController][onSubmit] - CheckYourAnswersSubmittedPage is true, resubmission blocked"
        )
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      } else {
        ValidatedSubcontractor.build(request.userAnswers) match {

          case Right(_) =>
            subcontractorService
              .createAndUpdateSubcontractor(request.userAnswers)
              .flatMap { _ =>
                auditService.addSubcontractorEvent(request.userAnswers)
                Future
                  .fromTry(request.userAnswers.set(CheckYourAnswersSubmittedPage, true))
                  .flatMap(updated => sessionRepository.set(updated).map(_ => ()))
                  .map(_ =>
                    Redirect(controllers.add.routes.SubcontractorAddedController.individualSubcontractorAdded())
                  )
              }
              .recover { case t =>
                logger.error("[CheckYourAnswersController.onSubmit] Failed to create/update subcontractor", t)
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              }

          case Left(error) =>
            logger.error(s"[CheckYourAnswersController.onSubmit] Validation failed: $error")
            Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
    }

}
