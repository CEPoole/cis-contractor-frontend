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
import models.{NormalMode, UserAnswers}
import models.verify.SelectedSubcontractors
import models.response.GetCurrentVerificationBatchResponse
import pages.verify.*
import play.api.Logging
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ContinueVerificationSubmissionController @Inject() (
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

  def onSubmit(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      request.userAnswers.get(CurrentVerificationBatchResponsePage) match {

        case None =>
          logger.error(
            "[ContinueVerificationSubmissionController][onSubmit] - CurrentVerificationBatchResponsePage missing from userAnswers"
          )
          Future.successful(
            Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
          )

        case Some(currentBatch) =>
          val currentIds  = currentBatchSubcontractorIds(currentBatch)
          val selectedIds = existingSelectedIds(request.userAnswers)

          if (currentIds.isEmpty) {

            logger.error("[ContinueVerificationSubmissionController][onSubmit] - current batch has no subcontractors")
            Future.successful(
              Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
            )

          } else if (selectedIds.nonEmpty) {

            if (selectedIds == currentIds) {
              Future.successful(
                Redirect(
                  controllers.verify.routes.CheckVerificationBatchReadinessController
                    .checkVerificationBatchReadiness(NormalMode)
                )
              )
            } else {
              logger.error(
                "[ContinueVerificationSubmissionController][onSubmit] - selected subcontractors do not match current batch"
              )
              Future.successful(
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              )
            }

          } else {

            val selectedSubcontractors =
              buildSelectedSubcontractors(currentBatch)

            if (selectedSubcontractors.map(_.id) != currentIds) {

              logger.error(
                "[ContinueVerificationSubmissionController][onSubmit] - selected subcontractors do not match current batch"
              )
              Future.successful(
                Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
              )

            } else {

              for {
                updatedAnswers <- Future.fromTry(
                                    request.userAnswers
                                      .remove(SelectSubcontractorPage)
                                      .flatMap(_.remove(VerifyYourSubcontractorsYesNoPage))
                                      .flatMap(
                                        _.set(
                                          ReverifyExistingSubcontractorsYesNoPage,
                                          true
                                        )
                                      )
                                      .flatMap(
                                        _.set(
                                          SelectSubcontractorsToReverifyPage,
                                          selectedSubcontractors
                                        )
                                      )
                                  )

                _ <- sessionRepository.set(updatedAnswers)

              } yield Redirect(
                controllers.verify.routes.CheckVerificationBatchReadinessController
                  .checkVerificationBatchReadiness(NormalMode)
              )
            }
          }
      }
    }

  private def existingSelectedIds(
    userAnswers: UserAnswers
  ): Set[String] = {

    val selectedToVerify =
      userAnswers
        .get(SelectSubcontractorPage)
        .getOrElse(Set.empty)
        .map(_.id)

    val selectedToReverify =
      userAnswers
        .get(SelectSubcontractorsToReverifyPage)
        .getOrElse(Set.empty)
        .map(_.id)

    selectedToVerify ++ selectedToReverify
  }

  private def currentBatchSubcontractorIds(
    batch: GetCurrentVerificationBatchResponse
  ): Set[String] =
    batch.verifications
      .flatMap(_.subcontractorId)
      .map(_.toString)
      .toSet

  private def buildSelectedSubcontractors(
    batch: GetCurrentVerificationBatchResponse
  )(implicit messages: Messages): Set[SelectedSubcontractors] = {

    val subcontractorsById =
      batch.subcontractors
        .map(subcontractor => subcontractor.subcontractorId -> subcontractor)
        .toMap

    batch.verifications.flatMap { verification =>
      verification.subcontractorId.flatMap { subcontractorId =>
        subcontractorsById
          .get(subcontractorId)
          .map { subcontractor =>
            SelectedSubcontractors(
              id = subcontractorId.toString,
              name = verification.subcontractorName
                .map(_.trim)
                .filter(_.nonEmpty)
                .getOrElse(subcontractor.displayName)
            )
          }
      }
    }.toSet
  }
}
