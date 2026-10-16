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

package models.verify

import models.requests.{CreateSubmissionForVerificationRequest, VerificationToUpdate}
import models.{SubcontractorCurrentVerification, VerificationCurrentVerification}
import pages.verify.CurrentVerificationBatchResponsePage
import play.api.i18n.Messages
import queries.CisIdQuery
import utils.VerifyEmailResolver
import viewmodels.verify.SubcontractorDisplay

object CreateSubmissionForVerificationRequestBuilder {

  private val OrganisationAgentId = "-"

  def build(
    ua: models.UserAnswers,
    isAgent: Boolean,
    agentReference: Option[String]
  )(implicit messages: Messages): Either[String, CreateSubmissionForVerificationRequest] =
    for {
      instanceId <- ua.get(CisIdQuery).toRight("CisIdQuery not found")
      current    <- ua.get(CurrentVerificationBatchResponsePage).toRight("CurrentVerificationBatchResponsePage not found")

      batchId  <- current.verificationBatch.map(_.verificationBatchId).toRight("verificationBatchId missing")
      batchRef <-
        current.verificationBatch.flatMap(_.verifBatchResourceRef).toRight("verificationBatchResourceRef missing")
      agentId  <- resolveAgentId(isAgent, agentReference)
    } yield {
      val email: Option[String] = VerifyEmailResolver.resolvedEmail(ua)

      val subsById: Map[Long, SubcontractorCurrentVerification] =
        current.subcontractors.map(s => s.subcontractorId -> s).toMap

      val verifications: Seq[VerificationToUpdate] =
        current.verifications.flatMap { verification =>
          verification.verificationResourceRef.map { ref =>
            VerificationToUpdate(
              subcontractorName = resolveName(verification, verification.subcontractorId.flatMap(subsById.get)),
              verificationResourceRef = ref,
              proceedVerification = proceedFlag(verification)
            )
          }
        }

      CreateSubmissionForVerificationRequest(
        instanceId = instanceId,
        verificationBatchId = batchId,
        verificationBatchResourceRef = batchRef,
        emailRecipient = email,
        irMarkGenerated = None,
        verifications = verifications,
        agentId = Some(agentId)
      )
    }

  private def resolveAgentId(isAgent: Boolean, agentReference: Option[String]): Either[String, String] =
    if (isAgent) {
      agentReference.map(_.trim).filter(_.nonEmpty).toRight("Agent reference missing")
    } else {
      Right(OrganisationAgentId)
    }

  // "Y" when the user chose to proceed without meeting the minimum data requirements (VERIFICATION.PROCEED), else "N".
  private def proceedFlag(verification: VerificationCurrentVerification): String =
    verification.proceed.map(_.trim.toUpperCase).filter(_.nonEmpty).getOrElse("N")

  // Name maps to VERIFICATION.SUBCONTRACTOR_NAME, falling back to the subcontractor's derived name.
  private def resolveName(
    verification: VerificationCurrentVerification,
    sub: Option[SubcontractorCurrentVerification]
  )(implicit messages: Messages): String =
    verification.subcontractorName
      .map(_.trim)
      .filter(_.nonEmpty)
      .orElse(sub.map(SubcontractorDisplay.displayName))
      .getOrElse(messages("verify.noName"))
}
