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

package controllers

import models.UserAnswers
import models.agent.AgentClientIdentifiers
import play.api.Logging
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import queries.CisIdQuery
import repositories.SessionRepository
import services.CisManageService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

/** Shared agent/client validation and setup performed by the journey entry points.
  *
  * Returns `Left(redirect)` when the request cannot proceed (missing agent client data, the agent is not authorised for
  * the client, or a downstream failure), otherwise `Right(updatedAnswers)` with the user answers updated by the
  * relevant setup step. Each caller supplies its own continuation for the success case.
  */
trait AgentClientChecks extends Logging {

  protected def cisManageService: CisManageService
  protected def sessionRepository: SessionRepository

  protected def withAgentClientChecks(
    userId: String,
    isAgent: Boolean,
    userAnswers: UserAnswers
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Future[Either[Result, UserAnswers]] =
    if (!isAgent) {
      for {
        updated <- cisManageService.ensureCisIdInUserAnswers(userAnswers)
        _       <- sessionRepository.set(updated)
      } yield Right(updated)
    } else {
      getAgentClient(userId).flatMap {
        case None =>
          logger.error("[AgentClientChecks] Missing agent client data")
          Future.successful(Left(recovery))

        case Some(AgentClientIdentifiers(uniqueId, ton, tor)) =>
          cisManageService
            .hasClient(ton.trim, tor.trim)
            .flatMap {
              case true  =>
                storeInstanceId(uniqueId.trim, userAnswers).map(Right(_))
              case false =>
                logger.error(s"[AgentClientChecks] hasClient=false for taxOfficeNumber=$ton taxOfficeReference=$tor")
                Future.successful(Left(recovery))
            }
            .recover { case NonFatal(e) =>
              logger.error(s"[AgentClientChecks] hasClient check failed: ${e.getMessage}", e)
              Left(recovery)
            }
      }
    }

  private def getAgentClient(userId: String)(implicit
    ec: ExecutionContext,
    hc: HeaderCarrier
  ): Future[Option[AgentClientIdentifiers]] =
    cisManageService
      .getAgentClient(userId)
      .map(_.map { data =>
        AgentClientIdentifiers(data.uniqueId, data.taxOfficeNumber, data.taxOfficeReference)
      })

  private def storeInstanceId(instanceId: String, userAnswers: UserAnswers)(implicit
    ec: ExecutionContext
  ): Future[UserAnswers] =
    for {
      updated <- Future.fromTry(userAnswers.set(CisIdQuery, instanceId))
      _       <- sessionRepository.set(updated)
    } yield updated

  private def recovery: Result =
    Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
}
