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
import pages.add.trust.*
import play.api.mvc.Call

import javax.inject.{Inject, Singleton}

@Singleton
class TrustNavigator @Inject() () extends NavigatorForJourney {

  override def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call = mode match {
    case NormalMode =>
      normalRoutes(page)(userAnswers)
    case CheckMode  =>
      checkRouteMap(page)(userAnswers)
    case AmendMode  =>
      amendRouteMap(page)(userAnswers)
  }

  private def cyaRoute(mode: Mode): Call = mode match {
    case AmendMode =>
      controllers.amend.trust.routes.AmendTrustCheckYourAnswersController.onPageLoad()
    case _         => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
  }

  private val normalRoutes: Page => UserAnswers => Call = {

    case TrustNamePage                   => _ => controllers.add.trust.routes.TrustAddressYesNoController.onPageLoad(NormalMode)
    case TrustAddressYesNoPage           => userAnswers => navigatorFromTrustAddressYesNoPage(NormalMode)(userAnswers)
    case AddTrustContactMethodsYesNoPage =>
      userAnswers => navigatorFromAddTrustContactMethodsYesNoPage(NormalMode)(userAnswers)
    case TrustContactMethodOptionsPage   =>
      userAnswers => nextSelectedContactMethodPageAfter(current = None)(userAnswers)
    case TrustEmailAddressPage           =>
      userAnswers => nextSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Email))(userAnswers)
    case TrustPhoneNumberPage            =>
      userAnswers => nextSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Phone))(userAnswers)
    case TrustMobileNumberPage           =>
      userAnswers => nextSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Mobile))(userAnswers)
    case TrustUtrYesNoPage               => userAnswers => navigatorFromTrustUtrYesNoPage(NormalMode)(userAnswers)
    case TrustUtrPage                    => _ => controllers.add.trust.routes.TrustWorksReferenceYesNoController.onPageLoad(NormalMode)
    case TrustWorksReferenceYesNoPage    =>
      userAnswers => navigatorFromTrustWorksReferenceYesNoPage(NormalMode)(userAnswers)
    case TrustWorksReferencePage         => _ => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
    case _                               => _ => routes.IndexController.onPageLoad()
  }

  private val checkRouteMap: Page => UserAnswers => Call = {

    case TrustNamePage                   => _ => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
    case TrustAddressYesNoPage           => navigatorFromTrustAddressYesNoPage(CheckMode)(_)
    case AddTrustContactMethodsYesNoPage => navigatorFromAddTrustContactMethodsYesNoPage(CheckMode)(_)
    case TrustContactMethodOptionsPage   => nextMissingSelectedContactMethodPageAfter(current = None)(_)
    case TrustEmailAddressPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Email))(_)
    case TrustPhoneNumberPage            =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Phone))(_)
    case TrustMobileNumberPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Mobile))(_)
    case TrustUtrYesNoPage               => navigatorFromTrustUtrYesNoPage(CheckMode)(_)
    case TrustUtrPage                    => _ => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
    case TrustWorksReferenceYesNoPage    => navigatorFromTrustWorksReferenceYesNoPage(CheckMode)(_)
    case TrustWorksReferencePage         => _ => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
    case _                               => _ => controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
  }

  private val amendRouteMap: Page => UserAnswers => Call = {
    case TrustAddressYesNoPage           => navigatorFromTrustAddressYesNoPage(AmendMode)(_)
    case AddTrustContactMethodsYesNoPage => navigatorFromAddTrustContactMethodsYesNoPage(AmendMode)(_)
    case TrustContactMethodOptionsPage   => nextMissingSelectedContactMethodPageAfter(current = None, AmendMode)(_)
    case TrustEmailAddressPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Email), AmendMode)(_)
    case TrustPhoneNumberPage            =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Phone), AmendMode)(_)
    case TrustMobileNumberPage           =>
      nextMissingSelectedContactMethodPageAfter(current = Some(ContactMethodOptions.Mobile), AmendMode)(_)
    case TrustWorksReferenceYesNoPage    => navigatorFromTrustWorksReferenceYesNoPage(AmendMode)(_)
    case TrustUtrYesNoPage               => navigatorFromTrustUtrYesNoPage(AmendMode)(_)
    case _                               => _ => cyaRoute(AmendMode)
  }

  private def navigatorFromTrustUtrYesNoPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(TrustUtrYesNoPage), mode) match {
      case (Some(true), NormalMode)             => controllers.add.trust.routes.TrustUtrController.onPageLoad(mode)
      case (Some(false), NormalMode)            =>
        controllers.add.trust.routes.TrustWorksReferenceYesNoController.onPageLoad(NormalMode)
      case (Some(true), CheckMode | AmendMode)  =>
        ua.get(TrustUtrPage)
          .fold(controllers.add.trust.routes.TrustUtrController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }
      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)
      case _                                    =>
        logger.error(s"[TrustNavigator][navigatorFromTrustUtrYesNoPage] - unexpected state, mode=$mode")
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromTrustAddressYesNoPage(mode: Mode)(ua: UserAnswers): Call =
    mode match {
      case AmendMode =>
        ua.get(TrustAddressYesNoPage) match {
          case Some(true)  =>
            controllers.add.trust.routes.TrustAddressController.redirectToAmendAddressLookup()
          case Some(false) =>
            cyaRoute(mode)
          case None        =>
            logger.error(
              "[TrustNavigator][navigatorFromTrustAddressYesNoPage] - TrustAddressYesNoPage missing from userAnswers"
            )
            controllers.routes.JourneyRecoveryController.onPageLoad()
        }
      case _         =>
        addressLookupYesNoRoute(
          mode,
          ua.get(TrustAddressYesNoPage),
          ua.get(TrustAddressPage).isDefined,
          onYes = controllers.add.trust.routes.TrustAddressController.redirectToAddressLookup(NormalMode, None),
          onYesChange = controllers.add.trust.routes.TrustAddressController
            .redirectToAddressLookup(NormalMode, Some(CheckMode.toString)),
          onNo = controllers.add.trust.routes.AddTrustContactMethodsYesNoController.onPageLoad(NormalMode),
          checkYourAnswers = controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
        )
    }

  private def navigatorFromTrustWorksReferenceYesNoPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(TrustWorksReferenceYesNoPage), mode) match {
      case (Some(true), NormalMode)             =>
        controllers.add.trust.routes.TrustWorksReferenceController.onPageLoad(NormalMode)
      case (Some(false), NormalMode)            =>
        controllers.add.trust.routes.TrustCheckYourAnswersController.onPageLoad()
      case (Some(true), CheckMode | AmendMode)  =>
        ua.get(TrustWorksReferencePage)
          .fold(controllers.add.trust.routes.TrustWorksReferenceController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }
      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)
      case _                                    =>
        logger.error(s"[TrustNavigator][navigatorFromTrustWorksReferenceYesNoPage] - unexpected state, mode=$mode")
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def navigatorFromAddTrustContactMethodsYesNoPage(mode: Mode)(ua: UserAnswers): Call =
    (ua.get(AddTrustContactMethodsYesNoPage), mode) match {

      case (Some(true), NormalMode) =>
        controllers.add.trust.routes.TrustContactMethodOptionsController.onPageLoad(NormalMode)

      case (Some(false), NormalMode) =>
        controllers.add.trust.routes.TrustUtrYesNoController.onPageLoad(NormalMode)

      case (Some(true), CheckMode | AmendMode) =>
        ua.get(TrustContactMethodOptionsPage)
          .fold(controllers.add.trust.routes.TrustContactMethodOptionsController.onPageLoad(mode)) { _ =>
            cyaRoute(mode)
          }

      case (Some(false), CheckMode | AmendMode) =>
        cyaRoute(mode)

      case _ =>
        logger.error(
          s"[TrustNavigator][navigatorFromAddTrustContactMethodsYesNoPage] - unexpected state, mode=$mode"
        )
        routes.JourneyRecoveryController.onPageLoad()
    }

  private def nextSelectedContactMethodPageAfter(
    current: Option[ContactMethodOptions]
  )(userAnswers: UserAnswers): Call =
    navigateFromContactMethodPage(current, userAnswers) { remaining =>
      remaining.headOption.fold {
        controllers.add.trust.routes.TrustUtrYesNoController.onPageLoad(NormalMode)
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
          "[TrustNavigator][navigateFromContactMethodPage] - ContactMethodOptions missing or empty"
        )
        routes.JourneyRecoveryController.onPageLoad()
      } { selectedContactMethods =>
        current match {
          case Some(currentContactMethod) if !selectedContactMethods.contains(currentContactMethod) =>
            logger.error(
              "[TrustNavigator][navigateFromContactMethodPage] - current contact method not selected"
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
    userAnswers.get(TrustContactMethodOptionsPage).map {
      ContactMethodOptions.ordered
    }

  private def contactMethodPageCall(contactMethod: ContactMethodOptions, mode: Mode): Call =
    contactMethod match {
      case ContactMethodOptions.Email  =>
        controllers.add.trust.routes.TrustEmailAddressController.onPageLoad(mode)
      case ContactMethodOptions.Phone  =>
        controllers.add.trust.routes.TrustPhoneNumberController.onPageLoad(mode)
      case ContactMethodOptions.Mobile =>
        controllers.add.trust.routes.TrustMobileNumberController.onPageLoad(mode)
    }

  private def contactMethodPage(contactMethod: ContactMethodOptions): QuestionPage[String] =
    contactMethod match {
      case ContactMethodOptions.Email  => TrustEmailAddressPage
      case ContactMethodOptions.Phone  => TrustPhoneNumberPage
      case ContactMethodOptions.Mobile => TrustMobileNumberPage
    }

  private def isMissingAnswer(contactMethod: ContactMethodOptions)(userAnswers: UserAnswers): Boolean =
    userAnswers.get(contactMethodPage(contactMethod)).isEmpty

}
