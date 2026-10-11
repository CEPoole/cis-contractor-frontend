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

import controllers.actions.IdentifierAction
import models.UserAnswers
import play.api.Logging
import navigation.finalvalidation.FinalValidationNavigator
import services.FinalValidationHandoffService
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

@Singleton
class FinalValidationHandoffController @Inject() (
  identify: IdentifierAction,
  finalValidationHandoffService: FinalValidationHandoffService,
  finalValidationNavigator: FinalValidationNavigator,
  val controllerComponents: MessagesControllerComponents
)(using ec: ExecutionContext)
    extends FrontendBaseController
    with Logging {

  def onPageLoad(handoffId: String): Action[AnyContent] = identify.async { implicit request =>

    val emptyUserAnswers = UserAnswers(request.userId)

    finalValidationHandoffService
      .prepareMonthlyReturnJourney(emptyUserAnswers, handoffId)
      .map {
        case Some((updatedAnswers, payload)) =>
          Redirect(finalValidationNavigator.startPage(payload.changeTarget, updatedAnswers))
        case None                            =>
          logger.error("[FinalValidationHandoffController][onPageLoad] - handoff journey not prepared")
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
      .recover { case NonFatal(ex) =>
        logger.error(s"[FinalValidationHandoffController] Error preparing monthly return journey: ${ex.getMessage}", ex)
        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
  }
}
