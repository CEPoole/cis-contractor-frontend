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

package navigation.add

import controllers.routes
import models.TypeOfSubcontractor.{Individualorsoletrader, Limitedcompany, Partnership, Trust}
import models.add.ValidatedSubcontractor
import models.add.company.ValidatedCompany
import models.add.partnership.ValidatedPartnership
import models.add.trust.ValidatedTrust
import models.{AmendMode, CheckMode, Mode, NormalMode, TypeOfSubcontractor, UserAnswers}
import navigation.NavigatorForJourney
import pages.Page
import pages.add.TypeOfSubcontractorPage
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class SharedNavigator @Inject() () extends NavigatorForJourney {

  override def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call = mode match {
    case NormalMode =>
      normalRoutes(page)(userAnswers)
    case CheckMode  =>
      checkRouteMap(page)(userAnswers)
    case AmendMode  =>
      logger.error(s"[SharedNavigator][nextPage] - AmendMode not supported for page=$page")
      routes.JourneyRecoveryController.onPageLoad()
  }

  private val normalRoutes: Page => UserAnswers => Call = {
    case TypeOfSubcontractorPage => userAnswers => navigatorFromTypeOfSubcontractorPage(NormalMode)(userAnswers)
    case _                       => _ => routes.IndexController.onPageLoad()
  }

  private val checkRouteMap: Page => UserAnswers => Call = {
    case TypeOfSubcontractorPage => navigatorFromTypeOfSubcontractorPage(CheckMode)(_)
    case _                       => _ => controllers.add.routes.CheckYourAnswersController.onPageLoad()
  }

  private def navigatorFromTypeOfSubcontractorPage(mode: Mode)(userAnswers: UserAnswers): Call =
    userAnswers.get(TypeOfSubcontractorPage) match {
      case None                    =>
        logger.error(
          "[SharedNavigator][navigatorFromTypeOfSubcontractorPage] - TypeOfSubcontractorPage missing"
        )
        routes.JourneyRecoveryController.onPageLoad()
      case Some(subcontractorType) =>
        mode match {
          case NormalMode => firstJourneyPageFor(subcontractorType)
          case _          =>
            // Only skip straight to CYA when the selected type's journey is already complete
            // (i.e. the type was not changed). If the type changed its answers are cleaned up,
            // leaving the journey incomplete, so the user is taken through the relevant pages.
            if (journeyComplete(subcontractorType, userAnswers)) {
              checkYourAnswersFor(subcontractorType)
            } else {
              firstJourneyPageFor(subcontractorType)
            }
        }
    }

  private def firstJourneyPageFor(subcontractorType: TypeOfSubcontractor): Call =
    subcontractorType match {
      case Individualorsoletrader => controllers.add.routes.IndividualNamesOptionsController.onPageLoad(NormalMode)
      case Limitedcompany         => controllers.add.company.routes.CompanyNameController.onPageLoad(NormalMode)
      case Partnership            => controllers.add.partnership.routes.PartnershipNameController.onPageLoad(NormalMode)
      case Trust                  => controllers.add.trust.routes.TrustNameController.onPageLoad(NormalMode)
    }

  private def checkYourAnswersFor(subcontractorType: TypeOfSubcontractor): Call =
    subcontractorType match {
      case Individualorsoletrader => controllers.add.routes.CheckYourAnswersController.onPageLoad()
      case Limitedcompany         => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
      case Partnership            => controllers.add.partnership.routes.PartnershipCheckYourAnswersController.onPageLoad()
      case Trust                  => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
    }

  private def journeyComplete(subcontractorType: TypeOfSubcontractor, userAnswers: UserAnswers): Boolean =
    subcontractorType match {
      case Individualorsoletrader => ValidatedSubcontractor.build(userAnswers).isRight
      case Limitedcompany         => ValidatedCompany.build(userAnswers).isRight
      case Partnership            => ValidatedPartnership.build(userAnswers).isRight
      case Trust                  => ValidatedTrust.build(userAnswers).isRight
    }

}
