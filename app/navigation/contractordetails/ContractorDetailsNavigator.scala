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

package navigation.contractordetails

import controllers.contractordetails.{routes => contractorDetailsRoutes}
import controllers.routes
import models.{AmendMode, CheckMode, Mode, NormalMode, UserAnswers}
import navigation.NavigatorForJourney
import pages.Page
import pages.contractordetails.*
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class ContractorDetailsNavigator @Inject() () extends NavigatorForJourney {

  override def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call =
    if (userAnswers.get(ContractorDetailsValidationTargetPage).isDefined && finalValidationPage(page)) {
      controllers.finalvalidations.routes.ContractorDetailsFinalValidationController.onPageLoad()
    } else {
      mode match {
        case NormalMode            => normalRoutes(page)(userAnswers)
        case CheckMode | AmendMode => checkRoutes(page)(userAnswers)
      }
    }

  private def checkAnswers: Call =
    contractorDetailsRoutes.ContractorDetailsCheckAnswersController.onPageLoad()

  private val normalRoutes: Page => UserAnswers => Call = {
    case ContractorUtrPage               => _ => contractorDetailsRoutes.AddSchemeNameYesNoController.onPageLoad(NormalMode)
    case AddSchemeNameYesNoPage          => nextForAddSchemeNameYesNo(NormalMode)
    case SchemeNamePage                  => _ => contractorDetailsRoutes.AddEmailAddressYesNoController.onPageLoad(NormalMode)
    case AddEmailAddressYesNoPage        => nextForAddEmailAddressYesNo(NormalMode)
    case EnterContractorEmailAddressPage => _ => checkAnswers
    case _                               => _ => routes.IndexController.onPageLoad()
  }

  private val checkRoutes: Page => UserAnswers => Call = {
    case AddSchemeNameYesNoPage          => nextForAddSchemeNameYesNo(CheckMode)
    case AddEmailAddressYesNoPage        => nextForAddEmailAddressYesNo(CheckMode)
    case ContractorUtrPage               => _ => checkAnswers
    case SchemeNamePage                  => _ => checkAnswers
    case EnterContractorEmailAddressPage => _ => checkAnswers
    case _                               => _ => routes.IndexController.onPageLoad()
  }

  private def nextForAddSchemeNameYesNo(mode: Mode)(userAnswers: UserAnswers): Call =
    (userAnswers.get(AddSchemeNameYesNoPage), mode) match {
      case (Some(true), NormalMode)             =>
        contractorDetailsRoutes.SchemeNameController.onPageLoad(NormalMode)
      case (Some(false), NormalMode)            =>
        contractorDetailsRoutes.AddEmailAddressYesNoController.onPageLoad(NormalMode)
      case (Some(true), CheckMode | AmendMode)  =>
        contractorDetailsRoutes.SchemeNameController.onPageLoad(CheckMode)
      case (Some(false), CheckMode | AmendMode) =>
        checkAnswers
      case _                                    =>
        logger.error(s"[ContractorDetailsNavigator][nextForAddSchemeNameYesNo] - answer missing or invalid, mode=$mode")
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def nextForAddEmailAddressYesNo(mode: Mode)(userAnswers: UserAnswers): Call =
    (userAnswers.get(AddEmailAddressYesNoPage), mode) match {
      case (Some(true), NormalMode)             =>
        contractorDetailsRoutes.EnterContractorEmailAddressController.onPageLoad(NormalMode)
      case (Some(false), NormalMode)            =>
        checkAnswers
      case (Some(true), CheckMode | AmendMode)  =>
        contractorDetailsRoutes.EnterContractorEmailAddressController.onPageLoad(CheckMode)
      case (Some(false), CheckMode | AmendMode) =>
        checkAnswers
      case _                                    =>
        logger.error(
          s"[ContractorDetailsNavigator][nextForAddEmailAddressYesNo] - answer missing or invalid, mode=$mode"
        )
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def finalValidationPage(page: Page): Boolean =
    page == ContractorUtrPage ||
      page == SchemeNamePage ||
      page == EnterContractorEmailAddressPage
}
