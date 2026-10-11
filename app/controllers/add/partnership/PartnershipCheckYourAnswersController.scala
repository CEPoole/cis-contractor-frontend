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
import models.add.partnership.ValidatedPartnership
import pages.add.CheckYourAnswersSubmittedPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{AuditService, SubcontractorService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.checkAnswers.add.TypeOfSubcontractorSummary
import viewmodels.checkAnswers.add.partnership.*
import viewmodels.govuk.summarylist.*
import views.html.add.partnership.PartnershipCheckYourAnswersView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class PartnershipCheckYourAnswersController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  subcontractorService: SubcontractorService,
  sessionRepository: SessionRepository,
  auditService: AuditService,
  view: PartnershipCheckYourAnswersView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] = (identify andThen getData andThen requireData) { implicit request =>
    val ua = request.userAnswers
    ValidatedPartnership.build(ua) match {
      case Right(_)    =>
        val list = SummaryListViewModel(
          rows = Seq(
            TypeOfSubcontractorSummary.row(ua),
            PartnershipNameSummary.row(ua),
            PartnershipAddressYesNoSummary.row(ua),
            PartnershipAddressSummary.row(ua),
            AddPartnershipContactMethodsYesNoSummary.row(ua),
            PartnershipContactMethodOptionsSummary.row(ua),
            PartnershipEmailAddressSummary.row(ua),
            PartnershipPhoneNumberSummary.row(ua),
            PartnershipMobileNumberSummary.row(ua),
            PartnershipHasUtrYesNoSummary.row(ua),
            PartnershipUniqueTaxpayerReferenceSummary.row(ua),
            PartnershipNominatedPartnerNameSummary.row(ua),
            PartnershipNominatedPartnerUtrYesNoSummary.row(ua),
            PartnershipNominatedPartnerUtrSummary.row(ua),
            PartnershipNominatedPartnerNinoYesNoSummary.row(ua),
            PartnershipNominatedPartnerNinoSummary.row(ua),
            PartnershipNominatedPartnerCrnYesNoSummary.row(ua),
            PartnershipNominatedPartnerCrnSummary.row(ua),
            PartnershipWorksReferenceNumberYesNoSummary.row(ua),
            PartnershipWorksReferenceNumberSummary.row(ua)
          ).flatten
        )

        Ok(view(list))
      case Left(error) =>
        logger.error(s"[PartnershipCheckYourAnswersController.onPageLoad] Failed to load the page: $error")
        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
    }
  }

  def onSubmit(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      if (request.userAnswers.get(CheckYourAnswersSubmittedPage).contains(true)) {
        logger.error("[PartnershipCheckYourAnswersController][onSubmit] - CheckYourAnswersSubmittedPage already true")
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      } else {

        ValidatedPartnership.build(request.userAnswers) match {

          case Right(_) =>
            subcontractorService
              .createAndUpdateSubcontractor(request.userAnswers)
              .flatMap { _ =>
                auditService.addSubcontractorEvent(request.userAnswers)
                Future
                  .fromTry(request.userAnswers.set(CheckYourAnswersSubmittedPage, true))
                  .flatMap(updated => sessionRepository.set(updated).map(_ => ()))
                  .map { _ =>
                    Redirect(controllers.add.routes.SubcontractorAddedController.partnershipSubcontractorAdded())
                  }
              }
              .recover { case t =>
                logger.error(
                  "[PartnershipCheckYourAnswersController.onSubmit] Failed to create/update subcontractor",
                  t
                )
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              }

          case Left(error) =>
            logger.error(s"[PartnershipCheckYourAnswersController.onSubmit] Validation failed: $error")
            Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
    }

}
