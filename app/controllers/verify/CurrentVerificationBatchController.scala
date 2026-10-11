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

import controllers.actions.{DataRequiredAction, DataRetrievalAction, IdentifierAction}
import models.{Mode, UserAnswers}
import models.response.GetCurrentVerificationBatchResponse
import models.validation.SubcontractorValidationFailure
import pages.validation.SubcontractorValidationFailuresPage
import pages.verify.{CurrentVerificationBatchResponsePage, SelectSubcontractorPage, SelectSubcontractorsToReverifyPage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, RequestHeader, Result}
import repositories.SessionRepository
import services.{SubcontractorDetailsValidator, SubcontractorPartnershipValidator, VerificationService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class CurrentVerificationBatchController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  verificationBatchService: VerificationService,
  subcontractorDetailsValidator: SubcontractorDetailsValidator,
  subcontractorPartnershipValidator: SubcontractorPartnershipValidator,
  sessionRepository: SessionRepository
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      verificationBatchService
        .refreshNewestVerificationBatch(request.userAnswers)
        .flatMap { updatedAnswers =>
          if (verificationBatchService.latestBatchCanBeModified(updatedAnswers)) {
            verificationBatchService
              .getCurrentVerificationBatch(updatedAnswers)
              .flatMap { currentAnswers =>
                currentAnswers
                  .get(CurrentVerificationBatchResponsePage)
                  .map { response =>
                    val validationFailures =
                      SubcontractorValidationFailure.merge(
                        subcontractorDetailsValidator.validate(
                          response.subcontractors
                        ),
                        subcontractorPartnershipValidator.validate(
                          response.subcontractors
                        )
                      )

                    for {
                      answersWithFailures <-
                        Future.fromTry(
                          currentAnswers.set(
                            SubcontractorValidationFailuresPage,
                            validationFailures
                          )
                        )

                      _ <- sessionRepository.set(
                             answersWithFailures
                           )
                    } yield redirectFor(response, mode)
                  }
                  .getOrElse {
                    logger.error(
                      "[CurrentVerificationBatchController][onPageLoad] - CurrentVerificationBatchResponsePage missing from userAnswers"
                    )
                    Future.successful(
                      Redirect(
                        controllers.routes.JourneyRecoveryController
                          .onPageLoad()
                      )
                    )
                  }
              }
          } else {
            createVerificationBatch(updatedAnswers, mode)
          }
        }
        .recover { case throwable =>
          logger.error(
            "[CurrentVerificationBatchController.onPageLoad] Failed to refresh, validate or persist the current verification batch",
            throwable
          )

          Redirect(
            controllers.routes.JourneyRecoveryController
              .onPageLoad()
          )
        }
    }

  private def redirectFor(
    response: GetCurrentVerificationBatchResponse,
    mode: Mode
  ): Result =
    if (
      response.verificationBatch.nonEmpty ||
      response.verifications.nonEmpty
    ) {
      Redirect(
        controllers.verify.routes.ModifyVerificationBatchAndVerificationsController
          .modifyVerificationBatch(mode)
      )
    } else {
      logger.error(
        "[CurrentVerificationBatchController][redirectFor] - current batch has no verification batch or verifications"
      )
      Redirect(
        controllers.routes.JourneyRecoveryController
          .onPageLoad()
      )
    }

  private def createVerificationBatch(
    userAnswers: UserAnswers,
    mode: Mode
  )(implicit request: RequestHeader): Future[Result] = {
    val verifyIdsRaw: Seq[String] =
      userAnswers
        .get(SelectSubcontractorPage)
        .map(_.toSeq.map(_.id))
        .getOrElse(Seq.empty)

    val reverifyIdsRaw: Seq[String] =
      userAnswers
        .get(SelectSubcontractorsToReverifyPage)
        .map(_.toSeq.map(_.id))
        .getOrElse(Seq.empty)

    val selectedIdsEither =
      for {
        verifyIds   <- parseIds("SelectSubcontractorPage", verifyIdsRaw)
        reverifyIds <- parseIds("SelectSubcontractorsToReverifyPage", reverifyIdsRaw)
      } yield (verifyIds ++ reverifyIds).distinct

    selectedIdsEither match {
      case Left(msg) =>
        logger.error(s"[CurrentVerificationBatchController.createVerificationBatch] $msg")
        Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))

      case Right(selectedIds) =>
        verificationBatchService
          .createVerificationBatchAndVerifications(
            userAnswers = userAnswers,
            selectedSubcontractorIds = selectedIds,
            actionIndicator = None
          )
          .map(_ =>
            Redirect(
              controllers.verify.routes.CheckVerificationBatchReadinessController
                .checkVerificationBatchReadiness(mode)
            )
          )
    }
  }

  private def parseIds(label: String, ids: Iterable[String]): Either[String, Seq[Long]] = {
    val parsed = ids.toSeq.distinct.map(_.trim).map(_.toLongOption)
    if (parsed.forall(_.isDefined)) Right(parsed.flatten)
    else Left(s"Invalid subcontractor id(s) found in $label")
  }
}
