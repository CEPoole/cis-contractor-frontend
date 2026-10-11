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

package connectors

import models.Scheme
import models.agent.GetClientListStatusResponse
import models.requests.*
import models.requests.CreateAndUpdateSubcontractorPayload.*
import models.response.*
import models.finalvalidation.*
import play.api.Logging
import play.api.http.Status.{NOT_FOUND, NO_CONTENT, OK}
import play.api.libs.json.{JsValue, Json, OFormat}
import play.api.libs.ws.JsonBodyWritables.writeableOf_JsValue
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.*
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure

final case class HasClientResponse(hasClient: Boolean)

object HasClientResponse {
  implicit val format: OFormat[HasClientResponse] = Json.format[HasClientResponse]
}

@Singleton
class ConstructionIndustrySchemeConnector @Inject() (config: ServicesConfig, http: HttpClientV2)(implicit
  ec: ExecutionContext
) extends HttpReadsInstances
    with Logging {

  private val cisBaseUrl: String = config.baseUrl("construction-industry-scheme") + "/cis"

  def prepopulateContractorKnownFacts(
    instanceId: String,
    taxOfficeNumber: String,
    taxOfficeReference: String
  )(implicit hc: HeaderCarrier): Future[Unit] =
    http
      .post(url"$cisBaseUrl/contractor-known-facts/prepopulate/$taxOfficeNumber/$taxOfficeReference/$instanceId")
      .execute[HttpResponse]
      .flatMap { response =>
        if (response.status / 100 == 2) {
          Future.unit
        } else {
          Future.failed(UpstreamErrorResponse(response.body, response.status, response.status))
        }
      }

  def getCisTaxpayer()(implicit hc: HeaderCarrier): Future[CisTaxpayerResponse] =
    http
      .get(url"$cisBaseUrl/taxpayer")
      .execute[CisTaxpayerResponse]
      .map { response =>
        logger.info(s"[ConstructionIndustrySchemeConnector][getCisTaxpayer] Response: $response")
        response
      }

  def createAndUpdateSubcontractor(
    payload: CreateAndUpdateSubcontractorPayload
  )(implicit hc: HeaderCarrier): Future[Unit] = {

    val jsonPayload: JsValue = payload match {
      case p: IndividualOrSoleTraderPayload => Json.toJson(p)
      case p: PartnershipPayload            => Json.toJson(p)
      case p: CompanyPayload                => Json.toJson(p)
      case p: TrustPayload                  => Json.toJson(p)
    }

    logger.info(
      s"[ConstructionIndustrySchemeConnector][createAndUpdateSubcontractor] Payload: $jsonPayload"
    )

    http
      .post(url"$cisBaseUrl/subcontractor/create-and-update")
      .withBody(jsonPayload)
      .execute[HttpResponse]
      .flatMap { response =>
        if (response.status == NO_CONTENT) {
          Future.successful(())
        } else {
          Future.failed(new RuntimeException(s"Update subcontractor failed, returned ${response.status}"))
        }
      }
  }

  def getSubcontractorUTRs(cisId: String)(implicit hc: HeaderCarrier): Future[GetSubcontractorUTRsResponse] = {
    logger.info(s"[ConstructionIndustrySchemeConnector][getSubcontractorUTRs] cisId: $cisId")

    http
      .get(url"$cisBaseUrl/subcontractors/utr/$cisId")
      .execute[GetSubcontractorUTRsResponse]
      .map { response =>
        logger.info(s"[ConstructionIndustrySchemeConnector][getSubcontractorUTRs] Response: $response")
        response
      }
  }

  def startClientList(using HeaderCarrier): Future[GetClientListStatusResponse] =
    http
      .post(url"$cisBaseUrl/agent/client-list/retrieval/start")
      .execute[GetClientListStatusResponse]

  def hasClient(taxOfficeNumber: String, taxOfficeReference: String)(implicit hc: HeaderCarrier): Future[Boolean] =
    http
      .get(url"$cisBaseUrl/agent/has-client/$taxOfficeNumber/$taxOfficeReference")
      .execute[HasClientResponse]
      .map(_.hasClient)

  def getAgentClient(userId: String)(implicit
    hc: HeaderCarrier
  ): Future[Option[JsValue]] =
    http
      .get(url"$cisBaseUrl/user-cache/agent-client/$userId")
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK        => Some(response.json)
          case NOT_FOUND => None
          case _         => throw new HttpException(response.body, response.status)
        }
      }

  def getNewestVerificationBatch(
    instanceId: String
  )(implicit hc: HeaderCarrier): Future[GetNewestVerificationBatchResponse] =
    http
      .get(url"$cisBaseUrl/verification-batch/newest/$instanceId")
      .execute[GetNewestVerificationBatchResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][getNewestVerificationBatch] instanceId=$instanceId - Response received"
        )
        response
      }

  def getCurrentVerificationBatch(
    instanceId: String
  )(implicit hc: HeaderCarrier): Future[GetCurrentVerificationBatchResponse] =
    http
      .get(url"$cisBaseUrl/verification-batch/current/$instanceId")
      .execute[GetCurrentVerificationBatchResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][getCurrentVerificationBatch] instanceId=$instanceId - Response received"
        )
        response
      }

  def getLastSubmittedVerificationBatch(
    instanceId: String
  )(implicit hc: HeaderCarrier): Future[GetLastSubmittedVerificationBatchResponse] =
    http
      .get(url"$cisBaseUrl/verification-batch/last/$instanceId")
      .execute[GetLastSubmittedVerificationBatchResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][getLastSubmittedVerificationBatch] instanceId=$instanceId - Response received"
        )
        response
      }
      .andThen { case Failure(ex) =>
        logger.error(
          s"[ConstructionIndustrySchemeConnector][getLastSubmittedVerificationBatch] instanceId=$instanceId - Request failed",
          ex
        )
      }

  def createVerificationBatchAndVerifications(
    request: CreateVerificationBatchAndVerificationsRequest
  )(implicit hc: HeaderCarrier): Future[CreateVerificationBatchAndVerificationsResponse] =
    http
      .post(url"$cisBaseUrl/verification-batch/create")
      .withBody(Json.toJson(request))
      .execute[CreateVerificationBatchAndVerificationsResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][createVerificationBatchAndVerifications] instanceId=${request.instanceId} - created/updated batch and verifications"
        )
        response
      }

  def modifyVerificationBatch(
    request: ModifyVerificationsRequest
  )(implicit hc: HeaderCarrier): Future[Unit] =
    http
      .post(url"$cisBaseUrl/verification-batch/modify")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { resp =>
        resp.status match {
          case NO_CONTENT | OK =>
            logger.info(
              s"[ConstructionIndustrySchemeConnector][modifyVerificationBatch] instanceId=${request.instanceId} - modified batch/verifications"
            )
            Future.successful(())
          case other           =>
            Future.failed(new RuntimeException(s"Modify verification batch failed, returned $other"))
        }
      }

  def deleteVerification(
    request: DeleteVerificationRequest
  )(implicit hc: HeaderCarrier): Future[DeleteVerificationResponse] =
    http
      .post(url"$cisBaseUrl/verification/delete")
      .withBody(Json.toJson(request))
      .execute[DeleteVerificationResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][deleteVerification] " +
            s"instanceId=${request.instanceId}, verificationResourceRef=${request.verificationResourceRef} - deleted verification"
        )
        response
      }

  def createSubmissionForVerification(
    request: CreateSubmissionForVerificationRequest
  )(implicit hc: HeaderCarrier): Future[CreateSubmissionForVerificationResponse] =
    http
      .post(url"$cisBaseUrl/verification/submission/create")
      .withBody(Json.toJson(request))
      .execute[CreateSubmissionForVerificationResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][createSubmissionForVerification] instanceId=${request.instanceId} - created submissionId=${response.submissionId}"
        )
        response
      }

  def getScheme(instanceId: String)(implicit hc: HeaderCarrier): Future[Scheme] =
    http
      .get(url"$cisBaseUrl/scheme/$instanceId")
      .execute[Scheme]

  def updateSchemeVersion(
    request: UpdateSchemeVersionRequest
  )(implicit hc: HeaderCarrier): Future[UpdateSchemeVersionResponse] =
    http
      .post(url"$cisBaseUrl/scheme/version-update")
      .withBody(Json.toJson(request))
      .execute[UpdateSchemeVersionResponse]

  def updateScheme(
    request: UpdateSchemeRequest
  )(implicit hc: HeaderCarrier): Future[Unit] =
    http
      .post(url"$cisBaseUrl/scheme/update")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { response =>
        response.status match {
          case NO_CONTENT | OK =>
            Future.successful(())
          case other           =>
            Future.failed(new RuntimeException(s"Update scheme failed, returned $other: ${response.body}"))
        }
      }

  def submitVerificationToChris(
    submissionId: Long,
    request: ChrisVerificationRequest
  )(implicit hc: HeaderCarrier): Future[ChrisSubmissionResponse] =
    http
      .post(url"$cisBaseUrl/submissions/$submissionId/submit-verification-to-chris")
      .withBody(Json.toJson(request))
      .execute[ChrisSubmissionResponse]

  def getSubmissionStatus(
    pollUrl: String,
    submissionId: String
  )(implicit hc: HeaderCarrier): Future[ChrisPollResponse] =
    http
      .get(url"$cisBaseUrl/submissions/verification/poll?submissionId=$submissionId&pollUrl=$pollUrl")
      .execute[ChrisPollResponse]

  def getSubcontractor(
    cisId: String,
    subbieResourceRef: Long
  )(implicit hc: HeaderCarrier): Future[GetSubcontractorResponse] = {
    logger.info(
      s"[ConstructionIndustrySchemeConnector][getSubcontractor] " +
        s"cisId=$cisId, subbieResourceRef=$subbieResourceRef"
    )

    http
      .get(url"$cisBaseUrl/subcontractor/$cisId/$subbieResourceRef")
      .execute[GetSubcontractorResponse]
      .map { response =>
        logger.info(
          s"[ConstructionIndustrySchemeConnector][getSubcontractor] " +
            s"Retrieved subcontractor for cisId=$cisId, subbieResourceRef=$subbieResourceRef"
        )

        response
      }
  }

  def updateContractorDetails(
    request: UpdateContractorSchemeParams
  )(implicit hc: HeaderCarrier): Future[Unit] = {

    logger.info(
      s"[ConstructionIndustrySchemeConnector][updateContractorDetails] Submitting contractor details for schemeId=${request.schemeId}"
    )

    http
      .post(url"$cisBaseUrl/contractor-details/update")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { response =>
        response.status match {
          case NO_CONTENT | OK =>
            logger.info(
              s"[ConstructionIndustrySchemeConnector][updateContractorDetails] schemeId=${request.schemeId} - contractor details updated"
            )

            Future.successful(())

          case other =>
            logger.error(
              s"[ConstructionIndustrySchemeConnector][updateContractorDetails] schemeId=${request.schemeId} - failed with status $other"
            )

            Future.failed(
              new RuntimeException(
                s"Update contractor details failed, returned $other"
              )
            )
        }
      }
  }

  def getSubcontractorList(cisId: String)(implicit hc: HeaderCarrier): Future[GetSubcontractorListResponse] = {
    logger.debug(s"[ConstructionIndustrySchemeConnector][getSubcontractorList] cisId=$cisId")

    http
      .get(url"$cisBaseUrl/subcontractors/$cisId")
      .execute[GetSubcontractorListResponse]
      .map { response =>
        logger.debug(
          s"[ConstructionIndustrySchemeConnector][getSubcontractorList] " +
            s"Retrieved ${response.subcontractors.size} subcontractors for cisId=$cisId"
        )
        response
      }
  }

  def getFinalValidationJourneyHandoff(
    journeyType: String,
    handoffId: String
  )(using hc: HeaderCarrier): Future[Option[FinalValidationHandoffPayload]] =
    http
      .get(url"$cisBaseUrl/journey-handoffs/$journeyType/$handoffId")
      .execute[Option[FinalValidationHandoffPayload]]

  def proceedInsufficientVerification(
    request: ProceedVerificationRequest
  )(implicit hc: HeaderCarrier): Future[Unit] =
    http
      .post(url"$cisBaseUrl/verification/proceed-with-insufficient-data")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { resp =>
        resp.status match {
          case NO_CONTENT =>
            logger.info(
              s"[ConstructionIndustrySchemeConnector][proceedInsufficientVerification] instanceId=${request.instanceId}"
            )
            Future.successful(())
          case other      =>
            Future.failed(
              UpstreamErrorResponse(s"proceedInsufficientVerification failed, returned $other", other, other)
            )
        }
      }

  def proceedUnmatchedVerification(
    request: ProceedVerificationRequest
  )(implicit hc: HeaderCarrier): Future[Unit] =
    http
      .post(url"$cisBaseUrl/verification/proceed-with-unmatched-data")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { resp =>
        resp.status match {
          case NO_CONTENT =>
            logger.info(
              s"[ConstructionIndustrySchemeConnector][proceedUnmatchedVerification] instanceId=${request.instanceId}"
            )
            Future.successful(())
          case other      =>
            Future.failed(
              UpstreamErrorResponse(s"proceedUnmatchedVerification failed, returned $other", other, other)
            )
        }
      }

  def updateSubcontractor(
    request: UpdateSubcontractorRequest
  )(implicit hc: HeaderCarrier): Future[Unit] = {

    logger.info(
      s"[ConstructionIndustrySchemeConnector][updateSubcontractor] " +
        s"cisId=${request.cisId}, " +
        s"subcontractorId=${request.subcontractor.subcontractorId}, " +
        s"subbieResourceRef=${request.subcontractor.subbieResourceRef}"
    )

    http
      .post(url"$cisBaseUrl/subcontractor/update")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { response =>
        response.status match {
          case NO_CONTENT | OK =>
            logger.info(
              s"[ConstructionIndustrySchemeConnector][updateSubcontractor] " +
                s"Updated subcontractor"
            )
            Future.successful(())

          case other =>
            Future.failed(new RuntimeException(s"Update subcontractor failed, returned $other: ${response.body}"))
        }
      }
  }

  def createFinalValidationDraft(
    request: CreateFinalValidationDraftRequest
  )(implicit hc: HeaderCarrier): Future[CreateFinalValidationDraftResponse] =
    http
      .post(url"$cisBaseUrl/final-validation/drafts")
      .withBody(Json.toJson(request))
      .execute[CreateFinalValidationDraftResponse]

  def getFinalValidationDraft(
    instanceId: String,
    draftId: String
  )(implicit hc: HeaderCarrier): Future[FinalValidationDraft] =
    http
      .get(url"$cisBaseUrl/final-validation/drafts/$instanceId/$draftId")
      .execute[FinalValidationDraft]

  def updateFinalValidationReadiness(
    instanceId: String,
    draftId: String,
    request: UpdateFinalValidationReadinessRequest
  )(implicit hc: HeaderCarrier): Future[FinalValidationDraft] =
    http
      .put(url"$cisBaseUrl/final-validation/drafts/$instanceId/$draftId/readiness")
      .withBody(Json.toJson(request))
      .execute[FinalValidationDraft]

  def updateFinalValidationCorrection(
    instanceId: String,
    draftId: String,
    request: UpdateFinalValidationCorrectionRequest
  )(implicit hc: HeaderCarrier): Future[FinalValidationDraft] =
    http
      .put(url"$cisBaseUrl/final-validation/drafts/$instanceId/$draftId/correction")
      .withBody(Json.toJson(request))
      .execute[FinalValidationDraft]

  def commitFinalValidationDraft(
    instanceId: String,
    draftId: String
  )(implicit hc: HeaderCarrier): Future[Unit] =
    http
      .post(url"$cisBaseUrl/final-validation/drafts/$instanceId/$draftId/commit")
      .execute[HttpResponse]
      .flatMap { response =>
        response.status match {
          case NO_CONTENT | OK =>
            Future.successful(())
          case _               =>
            Future.failed(UpstreamErrorResponse(response.body, response.status, response.status))
        }
      }

  def resetFinalValidationSubcontractor(
    instanceId: String,
    draftId: String,
    subcontractorId: Long
  )(implicit hc: HeaderCarrier): Future[FinalValidationDraft] =
    http
      .put(url"$cisBaseUrl/final-validation/drafts/$instanceId/$draftId/subcontractors/$subcontractorId/reset")
      .execute[FinalValidationDraft]

  def updateSubcontractorForEdit(
    request: UpdateSubcontractorForEditRequest
  )(implicit hc: HeaderCarrier): Future[Unit] = {

    logger.info(
      s"[ConstructionIndustrySchemeConnector][updateSubcontractorForEdit] " +
        s"cisId=${request.cisId}, " +
        s"subcontractorId=${request.subcontractor.subcontractorId}, " +
        s"subbieResourceRef=${request.subcontractor.subbieResourceRef}"
    )

    http
      .post(url"$cisBaseUrl/subcontractor/edit")
      .withBody(Json.toJson(request))
      .execute[HttpResponse]
      .flatMap { response =>
        response.status match {
          case NO_CONTENT | OK =>
            logger.info(
              s"[ConstructionIndustrySchemeConnector][updateSubcontractorForEdit] " +
                s"Updated subcontractor"
            )

            Future.successful(())

          case other =>
            Future.failed(
              new RuntimeException(
                s"Update subcontractor for edit failed, returned $other: ${response.body}"
              )
            )
        }
      }
  }

}
