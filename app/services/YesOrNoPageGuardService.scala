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

package services
import models.Mode
import pages.QuestionPage
import pages.add._
import pages.add.company._
import pages.add.partnership._
import pages.add.trust._
import play.api.mvc.Results.Redirect
import utils.LoggingUtil

import javax.inject.{Inject, Singleton}
import play.api.mvc.{Call, Result}

@Singleton
class YesOrNoPageGuardService @Inject() extends LoggingUtil {

  def yesOrNoPageRoute(
    continueRoute: Result,
    guardCheck: Option[Boolean],
    yesOrNoPage: QuestionPage[Boolean],
    mode: Mode
  ): Result =
    guardCheck match {
      case Some(true)  => continueRoute
      case Some(false) => Redirect(fetchRoute(yesOrNoPage, mode))
      case _           =>
        logger.error(s"[YesOrNoPageGuardService][yesOrNoPageRoute] - guard answer missing, page=$yesOrNoPage")
        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

    }

  private def fetchRoute(page: QuestionPage[Boolean], mode: Mode): Call = page match {
    case AddIndividualContactMethodsYesNoPage =>
      controllers.add.routes.AddIndividualContactMethodsYesNoController.onPageLoad(mode)
    case UniqueTaxpayerReferenceYesNoPage     =>
      controllers.add.routes.UniqueTaxpayerReferenceYesNoController.onPageLoad(mode)
    case NationalInsuranceNumberYesNoPage     =>
      controllers.add.routes.NationalInsuranceNumberYesNoController.onPageLoad(mode)
    case WorksReferenceNumberYesNoPage        => controllers.add.routes.WorksReferenceNumberYesNoController.onPageLoad(mode)

    case AddCompanyContactMethodsYesNoPage =>
      controllers.add.company.routes.AddCompanyContactMethodsYesNoController.onPageLoad(mode)
    case CompanyUtrYesNoPage               =>
      controllers.add.company.routes.CompanyUtrYesNoController.onPageLoad(mode)
    case CompanyCrnYesNoPage               =>
      controllers.add.company.routes.CompanyCrnYesNoController.onPageLoad(mode)
    case CompanyWorksReferenceYesNoPage    =>
      controllers.add.company.routes.CompanyWorksReferenceYesNoController.onPageLoad(mode)

    case AddPartnershipContactMethodsYesNoPage    =>
      controllers.add.partnership.routes.AddPartnershipContactMethodsYesNoController.onPageLoad(mode)
    case PartnershipWorksReferenceNumberYesNoPage =>
      controllers.add.partnership.routes.PartnershipWorksReferenceNumberYesNoController.onPageLoad(mode)
    case PartnershipNominatedPartnerUtrYesNoPage  =>
      controllers.add.partnership.routes.PartnershipNominatedPartnerUtrYesNoController.onPageLoad(mode)
    case PartnershipNominatedPartnerNinoYesNoPage =>
      controllers.add.partnership.routes.PartnershipNominatedPartnerNinoYesNoController.onPageLoad(mode)
    case PartnershipNominatedPartnerCrnYesNoPage  =>
      controllers.add.partnership.routes.PartnershipNominatedPartnerCrnYesNoController.onPageLoad(mode)
    case PartnershipHasUtrYesNoPage               =>
      controllers.add.partnership.routes.PartnershipHasUtrYesNoController.onPageLoad(mode)

    case AddTrustContactMethodsYesNoPage =>
      controllers.add.trust.routes.AddTrustContactMethodsYesNoController.onPageLoad(mode)
    case TrustUtrYesNoPage               => controllers.add.trust.routes.TrustUtrYesNoController.onPageLoad(mode)
    case TrustWorksReferenceYesNoPage    =>
      controllers.add.trust.routes.TrustWorksReferenceYesNoController.onPageLoad(mode)
    case _                               =>
      logger.error(s"[YesOrNoPageGuardService][fetchRoute] - navigator fallback reached for page=$page")
      controllers.routes.JourneyRecoveryController.onPageLoad()
  }

}
