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

package navigation

import play.api.Logging
import play.api.mvc.Call
import pages.Page
import models.{CheckMode, Mode, NormalMode, UserAnswers}

trait NavigatorForJourney extends Logging {
  def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call

  /** Shared routing for an "add an address?" Yes/No page that feeds into the Address Lookup Frontend journey. Identical
    * across subcontractor types; only the target calls differ.
    *
    * @param addressYesNo
    *   the user's Yes/No answer
    * @param addressEntered
    *   whether an address has already been captured (CheckMode short-circuit)
    * @param onYes
    *   ALF on-ramp for the standard flow
    * @param onYesChange
    *   ALF on-ramp for the change flow
    * @param onNo
    *   next page when no address is being added
    * @param checkYourAnswers
    *   the journey's Check Your Answers page
    */
  protected def addressLookupYesNoRoute(
    mode: Mode,
    addressYesNo: Option[Boolean],
    addressEntered: Boolean,
    onYes: => Call,
    onYesChange: => Call,
    onNo: => Call,
    checkYourAnswers: => Call
  ): Call =
    (addressYesNo, mode) match {
      case (Some(true), NormalMode)  => onYes
      case (Some(false), NormalMode) => onNo
      case (Some(true), CheckMode)   => if (addressEntered) checkYourAnswers else onYesChange
      case (Some(false), CheckMode)  => checkYourAnswers
      case _                         =>
        logger.error(s"[NavigatorForJourney][addressLookupYesNoRoute] - navigator fallback reached, mode=$mode")
        controllers.routes.JourneyRecoveryController.onPageLoad()
    }
}
