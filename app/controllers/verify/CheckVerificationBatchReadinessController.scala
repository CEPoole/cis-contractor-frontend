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

import controllers.actions.*
import models.{AmendMode, CheckMode, Mode, NormalMode, UserAnswers}
import models.verify.VerificationBatchReadiness
import pages.verify.{CurrentVerificationBatchResponsePage, NewestVerificationBatchResponsePage, SelectSubcontractorPage, SelectSubcontractorsToReverifyPage, VerificationBatchReadinessPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CheckVerificationBatchReadinessController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  sessionRepository: SessionRepository,
  val controllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def checkVerificationBatchReadiness(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      updateVerificationBatchReadiness(request.userAnswers).map {
        case Some(updatedAnswers) if updatedAnswers.get(VerificationBatchReadinessPage).contains(true) =>
          val redirect = mode match {
            case NormalMode =>
              nextEmailConfirmationPage(updatedAnswers)

            case CheckMode =>
              controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

            case AmendMode =>
              logger.error(
                "[CheckVerificationBatchReadinessController][checkVerificationBatchReadiness] - unexpected AmendMode"
              )
              controllers.routes.JourneyRecoveryController.onPageLoad()
          }

          Redirect(redirect)

        case Some(_) =>
          Redirect(controllers.verify.routes.ReviewInsufficientInfoSubcontractorsController.onPageLoad())

        case None =>
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def updateVerificationBatchReadiness(ua: UserAnswers): Future[Option[UserAnswers]] =
    batchReadiness(ua) match {
      case Some(batchReady) =>
        for {
          updatedAnswers <- Future.fromTry(ua.set(VerificationBatchReadinessPage, batchReady))
          _              <- sessionRepository.set(updatedAnswers)
        } yield Some(updatedAnswers)

      case None =>
        logger.error(
          "[CheckVerificationBatchReadinessController.updateVerificationBatchReadiness] Missing selected subcontractors or verification batch response"
        )
        Future.successful(None)
    }

  private def batchReadiness(ua: UserAnswers): Option[Boolean] = {
    val selectedUnverifiedIds: Set[String] =
      ua.get(SelectSubcontractorPage)
        .getOrElse(Set.empty)
        .map(_.id)

    val selectedReverifyIds: Set[String] =
      ua.get(SelectSubcontractorsToReverifyPage)
        .getOrElse(Set.empty)
        .map(_.id)

    val selectedIds: Set[String] =
      selectedUnverifiedIds ++ selectedReverifyIds

    ua.get(CurrentVerificationBatchResponsePage)
      .filter(_ => selectedIds.nonEmpty)
      .flatMap { batchResponse =>
        val currentSubcontractorIds =
          batchResponse.verifications
            .flatMap(_.subcontractorId)
            .map(_.toString)
            .toSet

        val remainingSelectedIds =
          selectedIds.intersect(currentSubcontractorIds)

        if (batchResponse.verifications.isEmpty) {
          Some(true)
        } else {
          Option.when(remainingSelectedIds.nonEmpty) {
            VerificationBatchReadiness.isBatchReady(remainingSelectedIds, batchResponse)
          }
        }
      }
  }

  private def nextEmailConfirmationPage(ua: UserAnswers): Call = {
    val hasStoredEmail = ua
      .get(NewestVerificationBatchResponsePage)
      .flatMap(_.scheme)
      .flatMap(_.emailAddress)
      .isDefined
    if (hasStoredEmail) {
      controllers.verify.routes.ContractorEmailConfirmationStoredController.onPageLoad(NormalMode)
    } else {
      controllers.verify.routes.ContractorEmailConfirmationNotStoredController.onPageLoad(NormalMode)
    }
  }
}
