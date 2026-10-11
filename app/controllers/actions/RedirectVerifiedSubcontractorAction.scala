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

package controllers.actions

import controllers.amend.AmendControllerUtils
import controllers.routes
import models.requests.DataRequest
import play.api.Logging
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionFilter, Result}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class RedirectVerifiedSubcontractorActionImpl @Inject() (implicit val executionContext: ExecutionContext)
    extends RedirectVerifiedSubcontractorAction
    with Logging {

  override protected def filter[A](request: DataRequest[A]): Future[Option[Result]] = {
    val showVerificationDetails = AmendControllerUtils.isVerifiedForAmendJourney(
      request.userAnswers
    )
    if (showVerificationDetails) {
      logger.error("[RedirectVerifiedSubcontractorActionImpl][filter] - subcontractor is verified for amend journey")
      Future.successful(Option(Redirect(routes.JourneyRecoveryController.onPageLoad())))
    } else {
      Future.successful(None)
    }
  }
}

trait RedirectVerifiedSubcontractorAction extends ActionFilter[DataRequest]
