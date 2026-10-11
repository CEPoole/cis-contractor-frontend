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

package controllers.add.trust

import controllers.actions.*
import models.add.trust.ValidatedTrust
import pages.add.CheckYourAnswersSubmittedPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{AuditService, SubcontractorService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import viewmodels.checkAnswers.add.TypeOfSubcontractorSummary
import viewmodels.checkAnswers.add.trust.*
import viewmodels.govuk.summarylist.*
import views.html.add.trust.TrustCheckYourAnswersView

import scala.concurrent.{ExecutionContext, Future}
import javax.inject.Inject

class TrustCheckYourAnswersController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  subcontractorService: SubcontractorService,
  sessionRepository: SessionRepository,
  auditService: AuditService,
  view: TrustCheckYourAnswersView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad: Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>
      val ua = request.userAnswers

      ValidatedTrust.build(ua) match {
        case Right(_)    =>
          val list = SummaryListViewModel(
            rows = Seq(
              TypeOfSubcontractorSummary.row(ua),
              TrustNameSummary.row(ua),
              TrustAddressYesNoSummary.row(ua),
              TrustAddressSummary.row(ua),
              AddTrustContactMethodsYesNoSummary.row(ua),
              TrustContactMethodOptionsSummary.row(ua),
              TrustEmailAddressSummary.row(ua),
              TrustPhoneNumberSummary.row(ua),
              TrustMobileNumberSummary.row(ua),
              TrustUtrYesNoSummary.row(ua),
              TrustUtrSummary.row(ua),
              TrustWorksReferenceYesNoSummary.row(ua),
              TrustWorksReferenceSummary.row(ua)
            ).flatten
          )
          Ok(view(list))
        case Left(error) =>
          logger.error(s"[TrustCheckYourAnswersController.onPageLoad] Failed to load the page: $error")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onSubmit(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      if (request.userAnswers.get(CheckYourAnswersSubmittedPage).contains(true)) {
        logger.error("[TrustCheckYourAnswersController][onSubmit] - CheckYourAnswersSubmittedPage already true")
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      } else {
        ValidatedTrust.build(request.userAnswers) match {
          case Right(_)    =>
            subcontractorService
              .createAndUpdateSubcontractor(request.userAnswers)
              .flatMap { _ =>
                auditService.addSubcontractorEvent(request.userAnswers)
                Future
                  .fromTry(request.userAnswers.set(CheckYourAnswersSubmittedPage, true))
                  .flatMap(updated => sessionRepository.set(updated).map(_ => ()))
                  .map { _ =>
                    Redirect(controllers.add.routes.SubcontractorAddedController.trustSubcontractorAdded())
                  }
              }
              .recover { case t =>
                logger.error(
                  "[TrustCheckYourAnswersController.onSubmit] Failed to create/update subcontractor",
                  t
                )
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              }
          case Left(error) =>
            logger.error(s"[TrustCheckYourAnswersController.onSubmit] Validation failed: $error")
            Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
      }
    }
}
