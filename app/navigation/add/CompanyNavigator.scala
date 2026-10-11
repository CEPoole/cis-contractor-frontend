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
import models.contact.ContactMethodOptions
import models.{AmendMode, CheckMode, Mode, NormalMode, UserAnswers}
import navigation.NavigatorForJourney
import pages.{Page, QuestionPage}
import pages.add.company.*
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class CompanyNavigator @Inject() () extends NavigatorForJourney {

  override def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call = mode match {
    case NormalMode =>
      normalRoutes(page)(userAnswers)
    case CheckMode  =>
      checkRouteMap(page)(userAnswers)
    case AmendMode  =>
      amendRouteMap(page)(userAnswers)
  }

  private def cyaRoute(mode: Mode): Call = mode match {
    case AmendMode => controllers.amend.company.routes.AmendCompanyCheckYourAnswersController.onPageLoad()
    case _         => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
  }

  private val normalRoutes: Page => UserAnswers => Call = {
    case CompanyNamePage                   => _ => controllers.add.company.routes.CompanyAddressYesNoController.onPageLoad(NormalMode)
    case CompanyAddressYesNoPage           =>
      userAnswers => navigatorFromCompanyAddressYesNoPage(NormalMode)(userAnswers)
    case AddCompanyContactMethodsYesNoPage =>
      userAnswers => navigatorFromAddCompanyContactMethodsYesNoPage(NormalMode)(userAnswers)
    case CompanyContactMethodOptionsPage   =>
      userAnswers => nextSelectedContactMethodPageAfter(current = None)(userAnswers)
    case CompanyEmailAddressPage           =>
      userAnswers => nextSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Email))(userAnswers)
    case CompanyPhoneNumberPage            =>
      userAnswers => nextSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Phone))(userAnswers)
    case CompanyMobileNumberPage           =>
      userAnswers => nextSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Mobile))(userAnswers)
    case CompanyUtrYesNoPage               =>
      userAnswers => navigatorFromCompanyUtrYesNoPage(NormalMode)(userAnswers)
    case CompanyUtrPage                    =>
      _ => controllers.add.company.routes.CompanyCrnYesNoController.onPageLoad(NormalMode)
    case CompanyCrnYesNoPage               =>
      userAnswers => navigatorFromCompanyCrnYesNoPage(NormalMode)(userAnswers)
    case CompanyCrnPage                    =>
      _ => controllers.add.company.routes.CompanyWorksReferenceYesNoController.onPageLoad(NormalMode)
    case CompanyWorksReferenceYesNoPage    =>
      userAnswers => navigatorFromCompanyWorksReferenceYesNoPage(NormalMode)(userAnswers)
    case CompanyWorksReferencePage         =>
      _ => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
    case _                                 => _ => routes.IndexController.onPageLoad()
  }

  private val amendRouteMap: Page => UserAnswers => Call = {
    case CompanyNamePage                   => _ => cyaRoute(AmendMode)
    case AddCompanyContactMethodsYesNoPage => navigatorFromAddCompanyContactMethodsYesNoPage(AmendMode)(_)
    case CompanyContactMethodOptionsPage   => nextMissingSelectedContactMethodPageAfter(current = None, AmendMode)(_)
    case CompanyEmailAddressPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Email), AmendMode)(_)
    case CompanyPhoneNumberPage            =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Phone), AmendMode)(_)
    case CompanyMobileNumberPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Mobile), AmendMode)(_)
    case CompanyCrnPage                    => _ => cyaRoute(AmendMode)
    case CompanyAddressYesNoPage           => navigatorFromCompanyAddressYesNoPage(AmendMode)(_)
    case CompanyCrnYesNoPage               =>
      userAnswers => navigatorFromCompanyCrnYesNoPage(AmendMode)(userAnswers)
    case CompanyUtrYesNoPage               =>
      userAnswers => navigatorFromCompanyUtrYesNoPage(AmendMode)(userAnswers)
    case CompanyUtrPage                    => _ => cyaRoute(AmendMode)
    case CompanyWorksReferenceYesNoPage    =>
      userAnswers => navigatorFromCompanyWorksReferenceYesNoPage(AmendMode)(userAnswers)
    case CompanyWorksReferencePage         => _ => cyaRoute(AmendMode)
    case _                                 => _ => cyaRoute(AmendMode)
  }

  private val checkRouteMap: Page => UserAnswers => Call = {
    case CompanyNamePage                   =>
      _ => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
    case CompanyAddressYesNoPage           => navigatorFromCompanyAddressYesNoPage(CheckMode)(_)
    case AddCompanyContactMethodsYesNoPage => navigatorFromAddCompanyContactMethodsYesNoPage(CheckMode)(_)
    case CompanyContactMethodOptionsPage   => nextMissingSelectedContactMethodPageAfter(current = None)(_)
    case CompanyEmailAddressPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Email))(_)
    case CompanyPhoneNumberPage            =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Phone))(_)
    case CompanyMobileNumberPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Mobile))(_)
    case CompanyUtrYesNoPage               =>
      userAnswers => navigatorFromCompanyUtrYesNoPage(CheckMode)(userAnswers)
    case CompanyUtrPage                    =>
      _ => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
    case CompanyCrnYesNoPage               => navigatorFromCompanyCrnYesNoPage(CheckMode)(_)
    case CompanyCrnPage                    =>
      _ => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
    case CompanyWorksReferenceYesNoPage    =>
      userAnswers => navigatorFromCompanyWorksReferenceYesNoPage(CheckMode)(userAnswers)
    case CompanyWorksReferencePage         =>
      _ => controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
    case _                                 => _ => controllers.add.routes.CheckYourAnswersController.onPageLoad()
  }

  private def navigatorFromCompanyAddressYesNoPage(mode: Mode)(userAnswers: UserAnswers): Call =
    mode match {
      case AmendMode =>
        userAnswers.get(CompanyAddressYesNoPage) match {
          case Some(true)  =>
            controllers.add.company.routes.CompanyAddressController.redirectToAmendAddressLookup()
          case Some(false) =>
            cyaRoute(mode)
          case None        =>
            logger.error(
              "[CompanyNavigator][navigatorFromCompanyAddressYesNoPage] - CompanyAddressYesNoPage missing"
            )
            controllers.routes.JourneyRecoveryController.onPageLoad()
        }
      case _         =>
        addressLookupYesNoRoute(
          mode,
          userAnswers.get(CompanyAddressYesNoPage),
          userAnswers.get(CompanyAddressPage).isDefined,
          onYes = controllers.add.company.routes.CompanyAddressController.redirectToAddressLookup(NormalMode, None),
          onYesChange = controllers.add.company.routes.CompanyAddressController
            .redirectToAddressLookup(NormalMode, Some(CheckMode.toString)),
          onNo = controllers.add.company.routes.AddCompanyContactMethodsYesNoController.onPageLoad(NormalMode),
          checkYourAnswers = controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
        )
    }

  private def navigatorFromCompanyUtrYesNoPage(mode: Mode)(userAnswers: UserAnswers): Call =
    (userAnswers.get(CompanyUtrYesNoPage), mode) match {
      case (Some(true), NormalMode)             =>
        controllers.add.company.routes.CompanyUtrController.onPageLoad(NormalMode)
      case (Some(false), NormalMode)            =>
        controllers.add.company.routes.CompanyCrnYesNoController.onPageLoad(NormalMode)
      case (Some(true), CheckMode | AmendMode)  =>
        userAnswers
          .get(CompanyUtrPage)
          .fold(controllers.add.company.routes.CompanyUtrController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }
      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)
      case _                                    =>
        logger.error(s"[CompanyNavigator][navigatorFromCompanyUtrYesNoPage] - unexpected state, mode=$mode")
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromCompanyCrnYesNoPage(mode: Mode)(userAnswers: UserAnswers): Call =
    (userAnswers.get(CompanyCrnYesNoPage), mode) match {
      case (Some(true), NormalMode)             =>
        controllers.add.company.routes.CompanyCrnController.onPageLoad(NormalMode)
      case (Some(false), NormalMode)            =>
        controllers.add.company.routes.CompanyWorksReferenceYesNoController.onPageLoad(NormalMode)
      case (Some(true), CheckMode | AmendMode)  =>
        userAnswers
          .get(CompanyCrnPage)
          .fold(controllers.add.company.routes.CompanyCrnController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }
      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)
      case _                                    =>
        logger.error(s"[CompanyNavigator][navigatorFromCompanyCrnYesNoPage] - unexpected state, mode=$mode")
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromCompanyWorksReferenceYesNoPage(mode: Mode)(userAnswers: UserAnswers): Call =
    (userAnswers.get(CompanyWorksReferenceYesNoPage), mode) match {
      case (Some(true), NormalMode)             =>
        controllers.add.company.routes.CompanyWorksReferenceController.onPageLoad(NormalMode)
      case (Some(false), NormalMode)            =>
        controllers.add.company.routes.CompanyCheckYourAnswersController.onPageLoad()
      case (Some(true), CheckMode | AmendMode)  =>
        userAnswers
          .get(CompanyWorksReferencePage)
          .fold(controllers.add.company.routes.CompanyWorksReferenceController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }
      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)
      case _                                    =>
        logger.error(
          s"[CompanyNavigator][navigatorFromCompanyWorksReferenceYesNoPage] - unexpected state, mode=$mode"
        )
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromAddCompanyContactMethodsYesNoPage(mode: Mode)(userAnswers: UserAnswers): Call =
    (userAnswers.get(AddCompanyContactMethodsYesNoPage), mode) match {
      case (Some(true), NormalMode) =>
        controllers.add.company.routes.CompanyContactMethodOptionsController.onPageLoad(NormalMode)

      case (Some(false), NormalMode) =>
        controllers.add.company.routes.CompanyUtrYesNoController.onPageLoad(NormalMode)

      case (Some(true), CheckMode | AmendMode) =>
        userAnswers
          .get(CompanyContactMethodOptionsPage)
          .fold(controllers.add.company.routes.CompanyContactMethodOptionsController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }

      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)

      case _ =>
        logger.error(
          s"[CompanyNavigator][navigatorFromAddCompanyContactMethodsYesNoPage] - unexpected state, mode=$mode"
        )
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def nextSelectedContactMethodPageAfter(
    current: Option[ContactMethodOptions]
  )(userAnswers: UserAnswers): Call =
    navigateFromContactMethodPage(current, userAnswers) { remaining =>
      remaining.headOption.fold {
        controllers.add.company.routes.CompanyUtrYesNoController.onPageLoad(NormalMode)
      }(contactMethodPageCall(_, NormalMode))
    }

  private def nextMissingSelectedContactMethodPageAfter(
    current: Option[ContactMethodOptions],
    mode: Mode = CheckMode
  )(userAnswers: UserAnswers): Call =
    navigateFromContactMethodPage(current, userAnswers) { remaining =>
      remaining
        .find(isMissingAnswer(_)(userAnswers))
        .map(contactMethodPageCall(_, mode))
        .getOrElse(
          cyaRoute(mode)
        )
    }

  private def navigateFromContactMethodPage(
    current: Option[ContactMethodOptions],
    userAnswers: UserAnswers
  )(terminalStep: Seq[ContactMethodOptions] => Call): Call =
    selectedContactMethodsInOrder(userAnswers)
      .filter(_.nonEmpty)
      .fold {
        logger.error(
          "[CompanyNavigator][navigateFromContactMethodPage] - ContactMethodOptions missing or empty"
        )
        routes.JourneyRecoveryController.onPageLoad()
      } { selectedContactMethods =>
        current match {
          case Some(currentContactMethod) if !selectedContactMethods.contains(currentContactMethod) =>
            logger.error(
              "[CompanyNavigator][navigateFromContactMethodPage] - current contact method not selected"
            )
            routes.JourneyRecoveryController.onPageLoad()

          case _ =>
            val remaining: Seq[ContactMethodOptions] =
              current match {
                case None =>
                  selectedContactMethods

                case Some(currentContactMethod) =>
                  val currentIndex = selectedContactMethods.indexWhere(_ == currentContactMethod)
                  selectedContactMethods.drop(currentIndex + 1)
              }

            terminalStep(remaining)
        }
      }

  private def selectedContactMethodsInOrder(userAnswers: UserAnswers): Option[Seq[ContactMethodOptions]] =
    userAnswers.get(CompanyContactMethodOptionsPage).map {
      ContactMethodOptions.ordered
    }

  private def contactMethodPageCall(contactMethod: ContactMethodOptions, mode: Mode): Call =
    contactMethod match {
      case ContactMethodOptions.Email  =>
        controllers.add.company.routes.CompanyEmailAddressController.onPageLoad(mode)
      case ContactMethodOptions.Phone  =>
        controllers.add.company.routes.CompanyPhoneNumberController.onPageLoad(mode)
      case ContactMethodOptions.Mobile =>
        controllers.add.company.routes.CompanyMobileNumberController.onPageLoad(mode)
    }

  private def contactMethodPage(contactMethod: ContactMethodOptions): QuestionPage[String] =
    contactMethod match {
      case ContactMethodOptions.Email  => CompanyEmailAddressPage
      case ContactMethodOptions.Phone  => CompanyPhoneNumberPage
      case ContactMethodOptions.Mobile => CompanyMobileNumberPage
    }

  private def isMissingAnswer(contactMethod: ContactMethodOptions)(userAnswers: UserAnswers): Boolean =
    userAnswers.get(contactMethodPage(contactMethod)).isEmpty

}
