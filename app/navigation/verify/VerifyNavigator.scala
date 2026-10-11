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

package navigation.verify

import controllers.routes
import jakarta.inject.Singleton
import models.{AmendMode, CheckMode, Mode, NormalMode, UserAnswers}
import navigation.NavigatorForJourney
import pages.Page
import pages.verify.*
import models.verify.ContractorEmailConfirmationStored.{CurrentEmail, DifferentEmail, DoNotSend}
import pages.insufficient.ProceedInsufficientSubcontractorNameYesNoPage
import pages.unmatched.ProceedSubcontractorVerifyRequestPage
import play.api.mvc.Call

import javax.inject.Inject

@Singleton
class VerifyNavigator @Inject() () extends NavigatorForJourney {

  override def nextPage(page: Page, mode: Mode, userAnswers: models.UserAnswers): Call = mode match {
    case NormalMode =>
      normalRoutes(page)(userAnswers)
    case CheckMode  =>
      checkRouteMap(page)(userAnswers)
    case AmendMode  =>
      logger.error(s"[VerifyNavigator][nextPage] - AmendMode not supported, page=$page")
      routes.JourneyRecoveryController.onPageLoad()
  }

  private def normalRoutes: Page => UserAnswers => Call = {

    case ContractorEmailConfirmationNotStoredPage         =>
      userAnswers => navigatorFromContractorEmailConfirmationNotStoredPage(NormalMode)(userAnswers)
    case SelectSubcontractorPage                          =>
      userAnswers => navigatorFromSelectSubcontractorPage(NormalMode)(userAnswers)
    case VerifyYourSubcontractorsYesNoPage                =>
      userAnswers => navigatorFromVerifyYourSubcontractorsYesNoPage(NormalMode)(userAnswers)
    case ReverifyExistingSubcontractorsYesNoPage          =>
      userAnswers => navigatorFromReverifyExistingSubcontractorsYesNoPage(NormalMode)(userAnswers)
    case ContractorEmailConfirmationStoredPage            =>
      userAnswers => navigatorFromContractorEmailConfirmationStoredPage(NormalMode)(userAnswers)
    case SelectSubcontractorsToReverifyPage               =>
      userAnswers => navigatorFromSelectSubcontractorsToReverifyPage(NormalMode)(userAnswers)
    case EmailAddressPage                                 =>
      _ => controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()
    case VerificationDeclarationPage                      =>
      _ => controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()
    case ProceedInsufficientSubcontractorNameYesNoPage(_) =>
      _ => controllers.verify.routes.ReviewInsufficientInfoSubcontractorsController.onPageLoad()
    case ProceedSubcontractorVerifyRequestPage(_)         =>
      _ => controllers.verify.routes.ReviewUnmatchedSubcontractorsController.onPageLoad()
    case unexpected                                       =>
      _ =>
        logger.error(s"[VerifyNavigator][normalRoutes] - navigator fallback reached, page=$unexpected")
        controllers.routes.JourneyRecoveryController.onPageLoad()
  }

  private def checkRouteMap: Page => UserAnswers => Call = {
    case ContractorEmailConfirmationNotStoredPage =>
      userAnswers => navigatorFromContractorEmailConfirmationNotStoredPage(CheckMode)(userAnswers)
    case SelectSubcontractorPage                  =>
      userAnswers => navigatorFromSelectSubcontractorPage(CheckMode)(userAnswers)
    case VerifyYourSubcontractorsYesNoPage        =>
      userAnswers => navigatorFromVerifyYourSubcontractorsYesNoPage(CheckMode)(userAnswers)
    case ReverifyExistingSubcontractorsYesNoPage  =>
      userAnswers => navigatorFromReverifyExistingSubcontractorsYesNoPage(CheckMode)(userAnswers)
    case ContractorEmailConfirmationStoredPage    =>
      userAnswers => navigatorFromContractorEmailConfirmationStoredPage(CheckMode)(userAnswers)
    case SelectSubcontractorsToReverifyPage       =>
      userAnswers => navigatorFromSelectSubcontractorsToReverifyPage(CheckMode)(userAnswers)
    case EmailAddressPage                         =>
      _ => controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()
    case VerificationDeclarationPage              =>
      _ => controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()
    case unexpected                               =>
      _ =>
        logger.error(s"[VerifyNavigator][checkRouteMap] - navigator fallback reached, page=$unexpected")
        controllers.routes.JourneyRecoveryController.onPageLoad()
  }

  private def navigatorFromContractorEmailConfirmationNotStoredPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(ContractorEmailConfirmationNotStoredPage), mode) match {

      case (Some(true), m) =>
        controllers.verify.routes.EmailAddressController.onPageLoad(m)

      case (Some(false), NormalMode) =>
        controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

      case (Some(false), CheckMode) =>
        controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

      case _ =>
        logger.error(
          s"[VerifyNavigator][navigatorFromContractorEmailConfirmationNotStoredPage] - answer missing, mode=$mode"
        )
        controllers.routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromSelectSubcontractorPage(mode: Mode)(ua: UserAnswers): Call =
    mode match {

      case NormalMode =>
        val hasSubcontractorsToReverify =
          ua.get(NewestVerificationBatchResponsePage).exists(_.subcontractors.exists(_.isVerified))

        if (hasSubcontractorsToReverify) {
          controllers.verify.routes.ReverifyExistingSubcontractorsYesNoController.onPageLoad(NormalMode)
        } else {
          controllers.verify.routes.CurrentVerificationBatchController.onPageLoad(NormalMode)
        }

      case CheckMode =>
        val noSubcontractorsSelected = ua.get(SelectSubcontractorPage).forall(_.isEmpty)

        val rebuildingFromWarning = ua.get(RebuildVerificationFromWarningPage).contains(true)

        val noReverificationSelected = ua.get(SelectSubcontractorsToReverifyPage).forall(_.isEmpty)

        if (rebuildingFromWarning) {
          controllers.verify.routes.ReverifyExistingSubcontractorsYesNoController.onPageLoad(CheckMode)
        } else if (noSubcontractorsSelected && noReverificationSelected) {
          controllers.verify.routes.NoSubcontractorsSelectedWarningController.onPageLoadCheckMode()
        } else {
          controllers.verify.routes.CurrentVerificationBatchController.onPageLoad(CheckMode)
        }

      case AmendMode =>
        logger.error("[VerifyNavigator][navigatorFromSelectSubcontractorPage] - AmendMode not supported")
        controllers.routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromReverifyExistingSubcontractorsYesNoPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(ReverifyExistingSubcontractorsYesNoPage), mode) match {

      case (Some(true), NormalMode) =>
        controllers.verify.routes.SelectSubcontractorsToReverifyController.onPageLoad(NormalMode)

      case (Some(true), CheckMode) =>
        if (ua.get(SelectSubcontractorsToReverifyPage).exists(_.nonEmpty)) {
          controllers.verify.routes.CheckVerificationBatchReadinessController
            .checkVerificationBatchReadiness(CheckMode)
        } else {
          controllers.verify.routes.SelectSubcontractorsToReverifyController.onPageLoad(CheckMode)
        }

      case (Some(false), NormalMode) =>
        if (ua.get(SelectSubcontractorPage).forall(_.isEmpty)) {
          controllers.verify.routes.NoSubcontractorsSelectedWarningController.onPageLoad()
        } else {
          controllers.verify.routes.CurrentVerificationBatchController.onPageLoad(NormalMode)
        }

      case (Some(false), CheckMode) =>
        if (ua.get(SelectSubcontractorPage).forall(_.isEmpty)) {
          controllers.verify.routes.NoSubcontractorsSelectedWarningController.onPageLoadCheckMode()
        } else {
          controllers.verify.routes.CurrentVerificationBatchController.onPageLoad(CheckMode)
        }

      case _ =>
        logger.error(
          s"[VerifyNavigator][navigatorFromReverifyExistingSubcontractorsYesNoPage] - answer missing, mode=$mode"
        )
        controllers.routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromVerifyYourSubcontractorsYesNoPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(VerifyYourSubcontractorsYesNoPage), mode) match {

      case (Some(true), _) =>
        controllers.verify.routes.SelectSubcontractorsToReverifyController.onPageLoad(mode)

      case (Some(false), NormalMode) =>
        controllers.routes.IndexController.onPageLoad()

      case (Some(false), CheckMode) =>
        controllers.verify.routes.NoSubcontractorsSelectedWarningController.onPageLoadCheckMode()

      case _ =>
        logger.error(s"[VerifyNavigator][navigatorFromVerifyYourSubcontractorsYesNoPage] - answer missing, mode=$mode")
        controllers.routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromSelectSubcontractorsToReverifyPage(mode: Mode)(ua: UserAnswers): Call = {

    val hasSelections =
      ua.get(SelectSubcontractorPage).exists(_.nonEmpty) ||
        ua.get(SelectSubcontractorsToReverifyPage).exists(_.nonEmpty)

    if (hasSelections) {
      mode match {
        case NormalMode =>
          controllers.verify.routes.CurrentVerificationBatchController.onPageLoad(NormalMode)

        case CheckMode =>
          controllers.verify.routes.CurrentVerificationBatchController.onPageLoad(CheckMode)

        case AmendMode =>
          logger.error("[VerifyNavigator][navigatorFromSelectSubcontractorsToReverifyPage] - AmendMode not supported")
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }
    } else {
      mode match {
        case NormalMode =>
          controllers.verify.routes.NoSubcontractorsSelectedWarningController.onPageLoad()

        case CheckMode =>
          controllers.verify.routes.NoSubcontractorsSelectedWarningController.onPageLoadCheckMode()

        case AmendMode =>
          logger.error("[VerifyNavigator][navigatorFromSelectSubcontractorsToReverifyPage] - AmendMode not supported")
          controllers.routes.JourneyRecoveryController.onPageLoad()
      }
    }
  }

  private def navigatorFromContractorEmailConfirmationStoredPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(ContractorEmailConfirmationStoredPage), mode) match {

      case (Some(CurrentEmail), NormalMode) =>
        controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

      case (Some(CurrentEmail), CheckMode) =>
        controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

      case (Some(DifferentEmail), m) =>
        controllers.verify.routes.EmailAddressController.onPageLoad(m)

      case (Some(DoNotSend), NormalMode) =>
        controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

      case (Some(DoNotSend), CheckMode) =>
        controllers.verify.routes.VerifyCheckYourAnswersController.onPageLoad()

      case _ =>
        logger.error(
          s"[VerifyNavigator][navigatorFromContractorEmailConfirmationStoredPage] - answer missing, mode=$mode"
        )
        controllers.routes.JourneyRecoveryController.onPageLoad()
    }
}
