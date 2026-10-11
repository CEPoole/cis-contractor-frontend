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

import controllers.actions.ClientListCheckRedirects.systemError
import models.audit.AuthFailureAuditEventModel
import models.requests.IdentifierRequest
import play.api.Logging
import play.api.mvc.{Request, Result}
import services.{AuditService, CisManageService}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@Singleton
class HasClientGuard @Inject() (
  cisManageService: CisManageService,
  auditService: AuditService
)(using ec: ExecutionContext)
    extends Logging {

  private[actions] def check[A](request: IdentifierRequest[A]): Future[Option[Result]] =
    given HeaderCarrier = HeaderCarrierConverter.fromRequestAndSession(request, request.session)
    given Request[?]    = request

    cisManageService
      .getAgentClient(request.userId)
      .flatMap {
        case None =>
          logger.error(s"[HasClientGuard] No client found for agent ${request.userId}")
          Future.successful(Some(systemError))

        case Some(client) =>
          val taxOfficeNumber    = client.taxOfficeNumber
          val taxOfficeReference = client.taxOfficeReference

          if (taxOfficeNumber.isEmpty || taxOfficeReference.isEmpty) {
            logger.error(s"[HasClientGuard] Missing tax office number/reference in agent client data")
            Future.successful(Some(systemError))
          } else {
            cisManageService
              .hasClient(taxOfficeNumber, taxOfficeReference)
              .flatMap {
                case true =>
                  Future.successful(None)

                case false =>
                  logger.error(s"[HasClientGuard] hasClient=false for instanceId: ${client.uniqueId}")
                  auditService
                    .sendEvent(
                      AuthFailureAuditEventModel(
                        agentUserId = request.userId,
                        taxOfficeNumber = taxOfficeNumber,
                        taxOfficeReference = taxOfficeReference,
                        clientUniqueId = client.uniqueId
                      )
                    )
                    .map(_ => Some(systemError))
                    .recover { case NonFatal(ex) =>
                      logger.error("[HasClientGuard] failed to send authoriseServiceGuardFailure audit", ex)
                      Some(systemError)
                    }
              }
          }
      }
      .recover { case NonFatal(ex) =>
        logger.error("[HasClientGuard] hasClient check failed", ex)
        Some(systemError)
      }
}
