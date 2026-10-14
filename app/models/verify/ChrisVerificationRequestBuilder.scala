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

import connectors.ConstructionIndustrySchemeConnector
import models.{EmployerReference, SubcontractorCurrentVerification, UserAnswers, VerificationCurrentVerification}
import models.requests.{ChrisVerificationRequest, VerificationDetails}
import pages.verify.CurrentVerificationBatchResponsePage
import play.api.i18n.Messages
import queries.CisIdQuery
import uk.gov.hmrc.http.HeaderCarrier
import utils.VerifyEmailResolver
import viewmodels.verify.SubcontractorDisplay

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class ChrisVerificationRequestBuilder @Inject() (
  cisConnector: ConstructionIndustrySchemeConnector
)(implicit ec: ExecutionContext) {

  def build(
    ua: UserAnswers,
    isAgent: Boolean,
    employerReference: EmployerReference
  )(implicit hc: HeaderCarrier, messages: Messages): Future[ChrisVerificationRequest] = {

    val cisIdFut = requireFromSession(
      ua.get(CisIdQuery),
      "CisIdQuery not found in session data"
    )

    for {
      cisId                       <- cisIdFut
      scheme                      <- cisConnector.getScheme(cisId)
      currentVerificationBatch    <- requireFromSession(
                                       ua.get(CurrentVerificationBatchResponsePage),
                                       "CurrentVerificationBatchResponsePage not found in session data"
                                     )
      utr                          = requireValue(scheme.utr, "UTR not found in scheme data")
      verificationBatch            = requireValue(currentVerificationBatch.verificationBatch, "Verification batch not found")
      verificationBatchResourceRef =
        requireValue(verificationBatch.verifBatchResourceRef, "Verification batch resource ref not found")
      aoRef                        = scheme.accountsOfficeReference
      verificationRefs             = currentVerificationBatch.verifications.flatMap(_.verificationResourceRef).toSet
      currentBatchSubcontractors   =
        currentVerificationBatch.subcontractors.filter(_.subbieResourceRef.exists(verificationRefs))
      subsById                     = currentVerificationBatch.subcontractors.map(s => s.subcontractorId -> s).toMap
    } yield ChrisVerificationRequest(
      instanceId = cisId,
      isAgent = isAgent,
      clientTaxOfficeNumber = employerReference.taxOfficeNumber,
      clientTaxOfficeRef = employerReference.taxOfficeReference,
      contractorUTR = utr,
      contractorAORef = aoRef,
      verificationBatchId = verificationBatch.verificationBatchId.toString,
      verificationBatchResourceRef = verificationBatchResourceRef.toString,
      emailRecipient = VerifyEmailResolver.resolvedEmail(ua),
      language = Some(messages.lang.code),
      subcontractors = currentBatchSubcontractors,
      verifications = currentVerificationBatch.verifications.map(v =>
        toVerificationDetails(v, v.subcontractorId.flatMap(subsById.get))
      )
    )
  }

  private def requireFromSession[A](valueOpt: Option[A], errorMsg: String): Future[A] =
    valueOpt match {
      case Some(value) => Future.successful(value)
      case None        => Future.failed(new RuntimeException(errorMsg))
    }

  private def requireValue[A](valueOpt: Option[A], errorMsg: String): A =
    valueOpt.getOrElse(throw new RuntimeException(errorMsg))

  private def toVerificationDetails(
    verification: VerificationCurrentVerification,
    sub: Option[SubcontractorCurrentVerification]
  )(implicit messages: Messages): VerificationDetails =
    VerificationDetails(
      subcontractorName = resolveName(verification, sub),
      verificationResourceRef = requireValue(
        verification.verificationResourceRef.map(_.toString),
        "Verification resource ref not found"
      ),
      proceedVerification = verification.proceed.exists(_.trim.equalsIgnoreCase("Y"))
    )

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
