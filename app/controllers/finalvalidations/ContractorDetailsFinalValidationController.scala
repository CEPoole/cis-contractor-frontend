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

package controllers.finalvalidations

import config.FrontendAppConfig
import controllers.AgentClientChecks
import controllers.actions.*
import models.NormalMode
import models.UserAnswers
import models.contractordetails.ContractorDetailsValidationTarget
import models.contractordetails.ContractorDetailsValidationTarget.*
import pages.contractordetails.ContractorDetailsValidationTargetPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import services.{CisManageService, ContractorDetailsFinalValidationService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.finalvalidations.ReviewContractorDetailsView
import viewmodels.contractordetails.{ContractorDetailsTaskViewModel, ReviewContractorDetailsViewModel}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ContractorDetailsFinalValidationController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  finalValidationService: ContractorDetailsFinalValidationService,
  override protected val cisManageService: CisManageService,
  override protected val sessionRepository: SessionRepository,
  view: ReviewContractorDetailsView
)(implicit ec: ExecutionContext, appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with AgentClientChecks
    with Logging {

  def startFileMonthlyReturn(): Action[AnyContent] =
    start(FileMonthlyReturn)

  def startFileNilReturn(): Action[AnyContent] =
    start(FileNilReturn)

  def startVerifySubcontractors(): Action[AnyContent] =
    start(VerifySubcontractors)

  private def start(target: ContractorDetailsValidationTarget): Action[AnyContent] =
    (identify andThen getData).async { implicit request =>
      val userAnswers = request.userAnswers.getOrElse(UserAnswers(request.userId))

      withAgentClientChecks(request.userId, request.isAgent, userAnswers)
        .flatMap {
          case Left(redirect)        =>
            Future.successful(redirect)
          case Right(checkedAnswers) =>
            finalValidationService
              .refreshAndValidate(checkedAnswers, target)
              .map { case (_, validation) =>
                if (validation.allComplete) {
                  redirectToTarget(target)
                } else {
                  Redirect(routes.ContractorDetailsFinalValidationController.onPageLoad())
                }
              }
        }
        .recover { case t =>
          logger.error("[ContractorDetailsFinalValidationController.start] Failed final contractor validation", t)
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }

  def onPageLoad(): Action[AnyContent] =
    (identify andThen getData andThen requireData) { implicit request =>
      request.userAnswers.get(ContractorDetailsValidationTargetPage) match {
        case Some(target) =>
          val validation = finalValidationService.validate(request.userAnswers)
          Ok(view(viewModel(validation, target)))

        case None =>
          logger.error("[ContractorDetailsFinalValidationController][onPageLoad] - validation target missing")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  def onContinue(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      request.userAnswers.get(ContractorDetailsValidationTargetPage) match {
        case Some(target) if finalValidationService.validate(request.userAnswers).allComplete =>
          finalValidationService
            .updateSchemeFromAnswers(request.userAnswers)
            .map(_ => redirectToTarget(target))
            .recover { case t =>
              logger.error(
                "[ContractorDetailsFinalValidationController.onContinue] Failed to update final contractor details",
                t
              )
              Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
            }

        case Some(_) =>
          Future.successful(Redirect(routes.ContractorDetailsFinalValidationController.onPageLoad()))

        case None =>
          logger.error("[ContractorDetailsFinalValidationController][onContinue] - validation target missing")
          Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
      }
    }

  private def viewModel(
    validation: models.contractordetails.ContractorDetailsFinalValidation,
    target: ContractorDetailsValidationTarget
  ): ReviewContractorDetailsViewModel =
    ReviewContractorDetailsViewModel(
      tasks = Seq(
        ContractorDetailsTaskViewModel(
          titleKey = "finalValidations.reviewContractorDetails.task.utr",
          statusKey = statusKey(validation.utrComplete),
          href = Option.when(!validation.utrComplete)(
            controllers.contractordetails.routes.ContractorUtrController.onPageLoad(NormalMode).url
          ),
          id = "contractor-utr"
        ),
        ContractorDetailsTaskViewModel(
          titleKey = "finalValidations.reviewContractorDetails.task.schemeName",
          statusKey = statusKey(validation.schemeNameComplete),
          href = Option.when(!validation.schemeNameComplete)(
            "/construction-industry-scheme/contractor-details/enter-contractors-scheme-name"
          ),
          id = "scheme-name"
        ),
        ContractorDetailsTaskViewModel(
          titleKey = "finalValidations.reviewContractorDetails.task.email",
          statusKey = statusKey(validation.emailComplete),
          href = Option.when(!validation.emailComplete)(
            controllers.contractordetails.routes.EnterContractorEmailAddressController.onPageLoad(NormalMode).url
          ),
          id = "contractor-email"
        )
      ),
      finalTask = ContractorDetailsTaskViewModel(
        titleKey = finalTaskTitleKey(target),
        statusKey =
          if (validation.allComplete) "finalValidations.reviewContractorDetails.status.incomplete"
          else "finalValidations.reviewContractorDetails.status.cannotStart",
        href = Option.when(validation.allComplete)(
          routes.ContractorDetailsFinalValidationController.onContinue().url
        ),
        id = "final-action"
      )
    )

  private def statusKey(complete: Boolean): String =
    if (complete) "finalValidations.reviewContractorDetails.status.complete"
    else "finalValidations.reviewContractorDetails.status.incomplete"

  private def finalTaskTitleKey(target: ContractorDetailsValidationTarget): String =
    target match {
      case FileMonthlyReturn | FileNilReturn                    =>
        "finalValidations.reviewContractorDetails.task.fileReturn"
      case VerifySubcontractors | ReviewUnmatchedSubcontractors =>
        "finalValidations.reviewContractorDetails.task.verifySubcontractors"
    }

  private def redirectToTarget(target: ContractorDetailsValidationTarget): Result =
    target match {
      case FileMonthlyReturn             =>
        Redirect(appConfig.fileStandardReturnUrl)
      case FileNilReturn                 =>
        Redirect(appConfig.fileNilReturnUrl)
      case VerifySubcontractors          =>
        Redirect(controllers.verify.routes.NewestVerificationBatchController.onPageLoad())
      case ReviewUnmatchedSubcontractors =>
        Redirect(controllers.verify.routes.ReviewUnmatchedSubcontractorsRoutingController.onPageLoad())
    }
}
