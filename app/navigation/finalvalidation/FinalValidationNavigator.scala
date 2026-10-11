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

package navigation.finalvalidation

import controllers.add.routes as soleTraderRoutes
import controllers.add.company.routes as companyRoutes
import controllers.add.partnership.routes as partnershipRoutes
import controllers.add.trust.routes as trustRoutes
import models.{FinalValidationMode, Mode, UserAnswers}
import models.finalvalidation.FinalValidationChangeTarget
import models.finalvalidation.FinalValidationChangeTarget.*
import models.TypeOfSubcontractor.*
import models.add.IndividualNamesOptions
import models.contact.ContactMethodOptions
import navigation.NavigatorForJourney
import pages.add.*
import pages.add.company.*
import pages.add.partnership.*
import pages.add.trust.*
import pages.Page
import pages.finalvalidation.FinalValidationChangeTargetPage
import play.api.Logging
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class FinalValidationNavigator @Inject() extends NavigatorForJourney with Logging {

  def startPage(target: FinalValidationChangeTarget, userAnswers: UserAnswers): Call =
    userAnswers.get(TypeOfSubcontractorPage) match {
      case Some(Individualorsoletrader) => soleTraderStartPage(target)
      case Some(Limitedcompany)         => companyStartPage(target)
      case Some(Partnership)            => partnershipStartPage(target)
      case Some(Trust)                  => trustStartPage(target)
      case None                         =>
        logger.error("[FinalValidationNavigator][startPage] - TypeOfSubcontractorPage missing from userAnswers")
        recovery
    }

  override def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call =
    val target =
      userAnswers.get(FinalValidationChangeTargetPage)

    logger.info(
      s"[FinalValidationNavigator][nextPage] " +
        s"page=$page mode=$mode target=$target"
    )

    mode match {
      case FinalValidationMode =>
        userAnswers.get(FinalValidationChangeTargetPage) match {
          case Some(Names)               =>
            individualNamesNextPage(page, userAnswers)
          case Some(AddressYesNo)        =>
            addressYesNoNextPage(page, userAnswers)
          case Some(ContactDetailsYesNo) =>
            contactDetailsYesNoNextPage(page, userAnswers)
          case Some(UtrYesNo) | Some(NinoYesNo) | Some(CrnYesNo) | Some(PartnerUtrYesNo) | Some(
                WorksReferenceNumberYesNo
              ) =>
            identifierYesNoNextPage(page, userAnswers)
          case Some(_)                   =>
            complete
          case None                      =>
            logger.error(
              "[FinalValidationNavigator][nextPage] " +
                "FinalValidationChangeTargetPage missing"
            )
            recovery
        }
      case _                   =>
        logger.error(
          s"[FinalValidationNavigator][nextPage] " +
            s"Unexpected mode=$mode"
        )
        recovery
    }

  private def complete: Call =
    controllers.finalvalidations.routes.FinalValidationCompleteController.onPageLoad()

  private def recovery: Call =
    controllers.routes.JourneyRecoveryController.onPageLoad()

  private def yesNoNextPage(
    answer: Option[Boolean],
    onYes: => Call
  ): Call =
    answer match {
      case Some(true)  => onYes
      case Some(false) => complete
      case None        =>
        logger.error("[FinalValidationNavigator][yesNoNextPage] - yes/no answer missing from userAnswers")
        recovery
    }

  private def identifierYesNoNextPage(page: Page, userAnswers: UserAnswers): Call =
    page match {
      // Individual / sole trader
      case UniqueTaxpayerReferenceYesNoPage         =>
        yesNoNextPage(
          userAnswers.get(UniqueTaxpayerReferenceYesNoPage),
          soleTraderRoutes.SubcontractorsUniqueTaxpayerReferenceController.onPageLoad(FinalValidationMode)
        )
      case NationalInsuranceNumberYesNoPage         =>
        yesNoNextPage(
          userAnswers.get(NationalInsuranceNumberYesNoPage),
          soleTraderRoutes.SubNationalInsuranceNumberController.onPageLoad(FinalValidationMode)
        )
      case WorksReferenceNumberYesNoPage            =>
        yesNoNextPage(
          userAnswers.get(WorksReferenceNumberYesNoPage),
          soleTraderRoutes.WorksReferenceNumberController.onPageLoad(FinalValidationMode)
        )

      // company
      case CompanyUtrYesNoPage                      =>
        yesNoNextPage(
          userAnswers.get(CompanyUtrYesNoPage),
          companyRoutes.CompanyUtrController.onPageLoad(FinalValidationMode)
        )
      case CompanyCrnYesNoPage                      =>
        yesNoNextPage(
          userAnswers.get(CompanyCrnYesNoPage),
          companyRoutes.CompanyCrnController.onPageLoad(FinalValidationMode)
        )
      case CompanyWorksReferenceYesNoPage           =>
        yesNoNextPage(
          userAnswers.get(CompanyWorksReferenceYesNoPage),
          companyRoutes.CompanyWorksReferenceController.onPageLoad(FinalValidationMode)
        )

      // Trust
      case TrustUtrYesNoPage                        =>
        yesNoNextPage(
          userAnswers.get(TrustUtrYesNoPage),
          trustRoutes.TrustUtrController.onPageLoad(FinalValidationMode)
        )
      case TrustWorksReferenceYesNoPage             =>
        yesNoNextPage(
          userAnswers.get(TrustWorksReferenceYesNoPage),
          trustRoutes.TrustWorksReferenceController.onPageLoad(FinalValidationMode)
        )

      // Partnership
      case PartnershipHasUtrYesNoPage               =>
        yesNoNextPage(
          userAnswers.get(PartnershipHasUtrYesNoPage),
          partnershipRoutes.PartnershipUniqueTaxpayerReferenceController.onPageLoad(FinalValidationMode)
        )
      case PartnershipNominatedPartnerUtrYesNoPage  =>
        yesNoNextPage(
          userAnswers.get(PartnershipNominatedPartnerUtrYesNoPage),
          partnershipRoutes.PartnershipNominatedPartnerUtrController.onPageLoad(FinalValidationMode)
        )
      case PartnershipNominatedPartnerNinoYesNoPage =>
        yesNoNextPage(
          userAnswers.get(PartnershipNominatedPartnerNinoYesNoPage),
          partnershipRoutes.PartnershipNominatedPartnerNinoController.onPageLoad(FinalValidationMode)
        )
      case PartnershipNominatedPartnerCrnYesNoPage  =>
        yesNoNextPage(
          userAnswers.get(PartnershipNominatedPartnerCrnYesNoPage),
          partnershipRoutes.PartnershipNominatedPartnerCrnController.onPageLoad(FinalValidationMode)
        )
      case PartnershipWorksReferenceNumberYesNoPage =>
        yesNoNextPage(
          userAnswers.get(PartnershipWorksReferenceNumberYesNoPage),
          partnershipRoutes.PartnershipWorksReferenceNumberController.onPageLoad(FinalValidationMode)
        )
      case _                                        =>
        complete
    }

  private def individualNamesNextPage(
    page: Page,
    userAnswers: UserAnswers
  ): Call = {

    val selected =
      userAnswers
        .get(IndividualNamesOptionsPage)
        .getOrElse(Set.empty)

    page match {
      case IndividualNamesOptionsPage =>
        if (selected.contains(IndividualNamesOptions.SubcontractorName)) {
          soleTraderRoutes.SubcontractorNameController.onPageLoad(FinalValidationMode)
        } else if (selected.contains(IndividualNamesOptions.TradingName)) {
          soleTraderRoutes.TradingNameOfSubcontractorController.onPageLoad(FinalValidationMode)
        } else {
          logger.error(
            "[FinalValidationNavigator][individualNamesNextPage] - names option missing"
          )
          recovery
        }

      case SubcontractorNamePage =>
        if (selected.contains(IndividualNamesOptions.TradingName)) {
          soleTraderRoutes.TradingNameOfSubcontractorController.onPageLoad(FinalValidationMode)
        } else {
          complete
        }

      case TradingNameOfSubcontractorPage =>
        complete

      case _ =>
        logger.error(s"[FinalValidationNavigator][individualNamesNextPage] - navigator fallback reached, page=$page")
        recovery
    }
  }

  private def addressYesNoNextPage(page: Page, userAnswers: UserAnswers): Call =
    page match {
      case SubAddressYesNoPage         =>
        yesNoNextPage(
          userAnswers.get(SubAddressYesNoPage),
          soleTraderRoutes.AddressOfSubcontractorController.redirectToAddressLookup(FinalValidationMode, None)
        )
      case CompanyAddressYesNoPage     =>
        yesNoNextPage(
          userAnswers.get(CompanyAddressYesNoPage),
          companyRoutes.CompanyAddressController.redirectToAddressLookup(FinalValidationMode, None)
        )
      case PartnershipAddressYesNoPage =>
        yesNoNextPage(
          userAnswers.get(PartnershipAddressYesNoPage),
          partnershipRoutes.PartnershipAddressController.redirectToAddressLookup(FinalValidationMode, None)
        )
      case TrustAddressYesNoPage       =>
        yesNoNextPage(
          userAnswers.get(TrustAddressYesNoPage),
          trustRoutes.TrustAddressController.redirectToAddressLookup(FinalValidationMode, None)
        )
      case _                           =>
        logger.error(s"[FinalValidationNavigator][addressYesNoNextPage] - navigator fallback reached, page=$page")
        recovery
    }

  private def contactDetailsYesNoNextPage(page: Page, userAnswers: UserAnswers): Call =
    page match {
      // Individual / sole trader
      case AddIndividualContactMethodsYesNoPage  =>
        yesNoNextPage(
          userAnswers.get(AddIndividualContactMethodsYesNoPage),
          soleTraderRoutes.IndividualContactMethodOptionsController.onPageLoad(FinalValidationMode)
        )
      case IndividualContactMethodOptionsPage    =>
        nextSelectedIndividualContactMethodPageAfter(
          current = None,
          userAnswers
        )
      case IndividualEmailAddressPage            =>
        nextSelectedIndividualContactMethodPageAfter(
          current = Some(ContactMethodOptions.Email),
          userAnswers
        )
      case IndividualPhoneNumberPage             =>
        nextSelectedIndividualContactMethodPageAfter(
          current = Some(ContactMethodOptions.Phone),
          userAnswers
        )
      case IndividualMobileNumberPage            =>
        nextSelectedIndividualContactMethodPageAfter(
          current = Some(ContactMethodOptions.Mobile),
          userAnswers
        )

      // company
      case AddCompanyContactMethodsYesNoPage     =>
        yesNoNextPage(
          userAnswers.get(AddCompanyContactMethodsYesNoPage),
          companyRoutes.CompanyContactMethodOptionsController.onPageLoad(FinalValidationMode)
        )
      case CompanyContactMethodOptionsPage       =>
        nextSelectedCompanyContactMethodPageAfter(
          current = None,
          userAnswers
        )
      case CompanyEmailAddressPage               =>
        nextSelectedCompanyContactMethodPageAfter(
          current = Some(ContactMethodOptions.Email),
          userAnswers
        )
      case CompanyPhoneNumberPage                =>
        nextSelectedCompanyContactMethodPageAfter(
          current = Some(ContactMethodOptions.Phone),
          userAnswers
        )
      case CompanyMobileNumberPage               =>
        nextSelectedCompanyContactMethodPageAfter(
          current = Some(ContactMethodOptions.Mobile),
          userAnswers
        )

      // Partnership
      case AddPartnershipContactMethodsYesNoPage =>
        yesNoNextPage(
          userAnswers.get(AddPartnershipContactMethodsYesNoPage),
          partnershipRoutes.PartnershipContactMethodOptionsController.onPageLoad(FinalValidationMode)
        )
      case PartnershipContactMethodOptionsPage   =>
        nextSelectedPartnershipContactMethodPageAfter(
          current = None,
          userAnswers
        )
      case PartnershipEmailAddressPage           =>
        nextSelectedPartnershipContactMethodPageAfter(
          current = Some(ContactMethodOptions.Email),
          userAnswers
        )
      case PartnershipPhoneNumberPage            =>
        nextSelectedPartnershipContactMethodPageAfter(
          current = Some(ContactMethodOptions.Phone),
          userAnswers
        )
      case PartnershipMobileNumberPage           =>
        nextSelectedPartnershipContactMethodPageAfter(
          current = Some(ContactMethodOptions.Mobile),
          userAnswers
        )

      // Trust
      case AddTrustContactMethodsYesNoPage       =>
        yesNoNextPage(
          userAnswers.get(AddTrustContactMethodsYesNoPage),
          trustRoutes.TrustContactMethodOptionsController.onPageLoad(FinalValidationMode)
        )
      case TrustContactMethodOptionsPage         =>
        nextSelectedTrustContactMethodPageAfter(
          current = None,
          userAnswers
        )
      case TrustEmailAddressPage                 =>
        nextSelectedTrustContactMethodPageAfter(
          current = Some(ContactMethodOptions.Email),
          userAnswers
        )
      case TrustPhoneNumberPage                  =>
        nextSelectedTrustContactMethodPageAfter(
          current = Some(ContactMethodOptions.Phone),
          userAnswers
        )
      case TrustMobileNumberPage                 =>
        nextSelectedTrustContactMethodPageAfter(
          current = Some(ContactMethodOptions.Mobile),
          userAnswers
        )
      case _                                     =>
        logger.error(
          s"[FinalValidationNavigator][contactDetailsYesNoNextPage] - navigator fallback reached, page=$page"
        )
        recovery
    }

  private def nextSelectedContactMethodPageAfter(
    current: Option[ContactMethodOptions],
    selectedContactMethods: Option[Seq[ContactMethodOptions]]
  )(contactMethodPageCall: ContactMethodOptions => Call): Call =
    navigateFromContactMethodPage(current, selectedContactMethods) { remaining =>
      remaining.headOption.fold {
        complete
      } { contactMethod =>
        contactMethodPageCall(contactMethod)
      }
    }

  private def navigateFromContactMethodPage(
    current: Option[ContactMethodOptions],
    selectedContactMethods: Option[Seq[ContactMethodOptions]]
  )(terminalStep: Seq[ContactMethodOptions] => Call): Call =
    selectedContactMethods
      .filter(_.nonEmpty)
      .fold {
        logger.error("[FinalValidationNavigator][navigateFromContactMethodPage] - contact methods missing or empty")
        recovery
      } { selected =>
        current match {
          case Some(currentContactMethod) if !selected.contains(currentContactMethod) =>
            logger.error(
              "[FinalValidationNavigator][navigateFromContactMethodPage] - current contact method not selected"
            )
            recovery
          case _                                                                      =>
            val remaining =
              current match {
                case None                       =>
                  selected
                case Some(currentContactMethod) =>
                  val currentIndex = selected.indexWhere(_ == currentContactMethod)
                  selected.drop(currentIndex + 1)
              }

            terminalStep(remaining)
        }
      }

  private def nextSelectedIndividualContactMethodPageAfter(
    current: Option[ContactMethodOptions],
    userAnswers: UserAnswers
  ): Call =
    nextSelectedContactMethodPageAfter(
      current,
      selectedIndividualContactMethodsInOrder(userAnswers)
    )(
      individualContactMethodPageCall
    )

  private def selectedIndividualContactMethodsInOrder(userAnswers: UserAnswers): Option[Seq[ContactMethodOptions]] =
    userAnswers
      .get(IndividualContactMethodOptionsPage)
      .map(ContactMethodOptions.ordered)

  private def individualContactMethodPageCall(contactMethod: ContactMethodOptions): Call =
    contactMethod match {
      case ContactMethodOptions.Email  =>
        soleTraderRoutes.IndividualEmailAddressController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Phone  =>
        soleTraderRoutes.IndividualPhoneNumberController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Mobile =>
        soleTraderRoutes.IndividualMobileNumberController.onPageLoad(FinalValidationMode)
    }

  private def nextSelectedCompanyContactMethodPageAfter(
    current: Option[ContactMethodOptions],
    userAnswers: UserAnswers
  ): Call =
    nextSelectedContactMethodPageAfter(
      current,
      selectedCompanyContactMethodsInOrder(userAnswers)
    )(
      companyContactMethodPageCall
    )

  private def selectedCompanyContactMethodsInOrder(userAnswers: UserAnswers): Option[Seq[ContactMethodOptions]] =
    userAnswers
      .get(CompanyContactMethodOptionsPage)
      .map(ContactMethodOptions.ordered)

  private def companyContactMethodPageCall(contactMethod: ContactMethodOptions): Call =
    contactMethod match {
      case ContactMethodOptions.Email  => companyRoutes.CompanyEmailAddressController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Phone  => companyRoutes.CompanyPhoneNumberController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Mobile => companyRoutes.CompanyMobileNumberController.onPageLoad(FinalValidationMode)
    }

  private def nextSelectedPartnershipContactMethodPageAfter(
    current: Option[ContactMethodOptions],
    userAnswers: UserAnswers
  ): Call =
    nextSelectedContactMethodPageAfter(
      current,
      selectedPartnershipContactMethodsInOrder(userAnswers)
    )(
      partnershipContactMethodPageCall
    )

  private def selectedPartnershipContactMethodsInOrder(userAnswers: UserAnswers): Option[Seq[ContactMethodOptions]] =
    userAnswers
      .get(PartnershipContactMethodOptionsPage)
      .map(ContactMethodOptions.ordered)

  private def partnershipContactMethodPageCall(contactMethod: ContactMethodOptions): Call =
    contactMethod match {
      case ContactMethodOptions.Email  =>
        partnershipRoutes.PartnershipEmailAddressController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Phone  =>
        partnershipRoutes.PartnershipPhoneNumberController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Mobile =>
        partnershipRoutes.PartnershipMobileNumberController.onPageLoad(FinalValidationMode)
    }

  private def nextSelectedTrustContactMethodPageAfter(
    current: Option[ContactMethodOptions],
    userAnswers: UserAnswers
  ): Call =
    nextSelectedContactMethodPageAfter(
      current,
      selectedTrustContactMethodsInOrder(userAnswers)
    )(
      trustContactMethodPageCall
    )

  private def selectedTrustContactMethodsInOrder(userAnswers: UserAnswers): Option[Seq[ContactMethodOptions]] =
    userAnswers
      .get(TrustContactMethodOptionsPage)
      .map(ContactMethodOptions.ordered)

  private def trustContactMethodPageCall(contactMethod: ContactMethodOptions): Call =
    contactMethod match {
      case ContactMethodOptions.Email  => trustRoutes.TrustEmailAddressController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Phone  => trustRoutes.TrustPhoneNumberController.onPageLoad(FinalValidationMode)
      case ContactMethodOptions.Mobile => trustRoutes.TrustMobileNumberController.onPageLoad(FinalValidationMode)
    }

  private def soleTraderStartPage(target: FinalValidationChangeTarget): Call =
    target match {
      case Names                     => soleTraderRoutes.IndividualNamesOptionsController.onPageLoad(FinalValidationMode)
      case SubcontractorName         => soleTraderRoutes.SubcontractorNameController.onPageLoad(FinalValidationMode)
      case TradingName               => soleTraderRoutes.TradingNameOfSubcontractorController.onPageLoad(FinalValidationMode)
      case AddressYesNo              => soleTraderRoutes.SubAddressYesNoController.onPageLoad(FinalValidationMode)
      case Address                   =>
        soleTraderRoutes.AddressOfSubcontractorController.redirectToAddressLookup(FinalValidationMode, None)
      case ContactDetailsYesNo       =>
        soleTraderRoutes.AddIndividualContactMethodsYesNoController.onPageLoad(FinalValidationMode)
      case EmailAddress              => soleTraderRoutes.IndividualEmailAddressController.onPageLoad(FinalValidationMode)
      case PhoneNumber               => soleTraderRoutes.IndividualPhoneNumberController.onPageLoad(FinalValidationMode)
      case MobilePhoneNumber         => soleTraderRoutes.IndividualMobileNumberController.onPageLoad(FinalValidationMode)
      case UtrYesNo                  => soleTraderRoutes.UniqueTaxpayerReferenceYesNoController.onPageLoad(FinalValidationMode)
      case Utr                       => soleTraderRoutes.SubcontractorsUniqueTaxpayerReferenceController.onPageLoad(FinalValidationMode)
      case NinoYesNo                 => soleTraderRoutes.NationalInsuranceNumberYesNoController.onPageLoad(FinalValidationMode)
      case Nino                      => soleTraderRoutes.SubNationalInsuranceNumberController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumberYesNo =>
        soleTraderRoutes.WorksReferenceNumberYesNoController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumber      => soleTraderRoutes.WorksReferenceNumberController.onPageLoad(FinalValidationMode)
      case _                         =>
        logger.error(s"[FinalValidationNavigator][soleTraderStartPage] - navigator fallback reached, target=$target")
        recovery
    }

  private def companyStartPage(target: FinalValidationChangeTarget): Call =
    target match {
      case TradingName               => companyRoutes.CompanyNameController.onPageLoad(FinalValidationMode)
      case AddressYesNo              => companyRoutes.CompanyAddressYesNoController.onPageLoad(FinalValidationMode)
      case Address                   => companyRoutes.CompanyAddressController.redirectToAddressLookup(FinalValidationMode, None)
      case ContactDetailsYesNo       => companyRoutes.AddCompanyContactMethodsYesNoController.onPageLoad(FinalValidationMode)
      case EmailAddress              => companyRoutes.CompanyEmailAddressController.onPageLoad(FinalValidationMode)
      case PhoneNumber               => companyRoutes.CompanyPhoneNumberController.onPageLoad(FinalValidationMode)
      case MobilePhoneNumber         => companyRoutes.CompanyMobileNumberController.onPageLoad(FinalValidationMode)
      case UtrYesNo                  => companyRoutes.CompanyUtrYesNoController.onPageLoad(FinalValidationMode)
      case Utr                       => companyRoutes.CompanyUtrController.onPageLoad(FinalValidationMode)
      case CrnYesNo                  => companyRoutes.CompanyCrnYesNoController.onPageLoad(FinalValidationMode)
      case Crn                       => companyRoutes.CompanyCrnController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumberYesNo =>
        companyRoutes.CompanyWorksReferenceYesNoController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumber      => companyRoutes.CompanyWorksReferenceController.onPageLoad(FinalValidationMode)
      case _                         =>
        logger.error(s"[FinalValidationNavigator][companyStartPage] - navigator fallback reached, target=$target")
        recovery
    }

  private def trustStartPage(target: FinalValidationChangeTarget): Call =
    target match {
      case TradingName               => trustRoutes.TrustNameController.onPageLoad(FinalValidationMode)
      case AddressYesNo              => trustRoutes.TrustAddressYesNoController.onPageLoad(FinalValidationMode)
      case Address                   => trustRoutes.TrustAddressController.redirectToAddressLookup(FinalValidationMode, None)
      case ContactDetailsYesNo       => trustRoutes.AddTrustContactMethodsYesNoController.onPageLoad(FinalValidationMode)
      case EmailAddress              => trustRoutes.TrustEmailAddressController.onPageLoad(FinalValidationMode)
      case PhoneNumber               => trustRoutes.TrustPhoneNumberController.onPageLoad(FinalValidationMode)
      case MobilePhoneNumber         => trustRoutes.TrustMobileNumberController.onPageLoad(FinalValidationMode)
      case UtrYesNo                  => trustRoutes.TrustUtrYesNoController.onPageLoad(FinalValidationMode)
      case Utr                       => trustRoutes.TrustUtrController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumberYesNo => trustRoutes.TrustWorksReferenceYesNoController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumber      => trustRoutes.TrustWorksReferenceController.onPageLoad(FinalValidationMode)
      case _                         =>
        logger.error(s"[FinalValidationNavigator][trustStartPage] - navigator fallback reached, target=$target")
        recovery
    }

  private def partnershipStartPage(target: FinalValidationChangeTarget): Call =
    target match {
      case PartnershipTradingName    => partnershipRoutes.PartnershipNameController.onPageLoad(FinalValidationMode)
      case TradingName               => partnershipRoutes.PartnershipNominatedPartnerNameController.onPageLoad(FinalValidationMode)
      case AddressYesNo              => partnershipRoutes.PartnershipAddressYesNoController.onPageLoad(FinalValidationMode)
      case Address                   => partnershipRoutes.PartnershipAddressController.redirectToAddressLookup(FinalValidationMode, None)
      case ContactDetailsYesNo       =>
        partnershipRoutes.AddPartnershipContactMethodsYesNoController.onPageLoad(FinalValidationMode)
      case EmailAddress              => partnershipRoutes.PartnershipEmailAddressController.onPageLoad(FinalValidationMode)
      case PhoneNumber               => partnershipRoutes.PartnershipPhoneNumberController.onPageLoad(FinalValidationMode)
      case MobilePhoneNumber         => partnershipRoutes.PartnershipMobileNumberController.onPageLoad(FinalValidationMode)
      case UtrYesNo                  => partnershipRoutes.PartnershipHasUtrYesNoController.onPageLoad(FinalValidationMode)
      case Utr                       => partnershipRoutes.PartnershipUniqueTaxpayerReferenceController.onPageLoad(FinalValidationMode)
      case PartnerUtrYesNo           =>
        partnershipRoutes.PartnershipNominatedPartnerUtrYesNoController.onPageLoad(FinalValidationMode)
      case PartnerUtr                => partnershipRoutes.PartnershipNominatedPartnerUtrController.onPageLoad(FinalValidationMode)
      case NinoYesNo                 => partnershipRoutes.PartnershipNominatedPartnerNinoYesNoController.onPageLoad(FinalValidationMode)
      case Nino                      => partnershipRoutes.PartnershipNominatedPartnerNinoController.onPageLoad(FinalValidationMode)
      case CrnYesNo                  => partnershipRoutes.PartnershipNominatedPartnerCrnYesNoController.onPageLoad(FinalValidationMode)
      case Crn                       => partnershipRoutes.PartnershipNominatedPartnerCrnController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumberYesNo =>
        partnershipRoutes.PartnershipWorksReferenceNumberYesNoController.onPageLoad(FinalValidationMode)
      case WorksReferenceNumber      =>
        partnershipRoutes.PartnershipWorksReferenceNumberController.onPageLoad(FinalValidationMode)
      case _                         =>
        logger.error(
          s"[FinalValidationNavigator][partnershipStartPage] - navigator fallback reached, target=$target"
        )
        recovery
    }

}
