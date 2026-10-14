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

import com.github.tomakehurst.wiremock.client.WireMock.*
import itutil.ApplicationWithWiremock
import models.agent.ClientListStatus
import models.TypeOfSubcontractor.Individualorsoletrader
import models.finalvalidation.*
import models.requests.*
import models.requests.CreateAndUpdateSubcontractorPayload.IndividualOrSoleTraderPayload
import models.response.*
import models.verify.SubmissionStatus
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.shouldBe
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.Status.*
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.{HeaderCarrier, HttpException, UpstreamErrorResponse}

class ConstructionIndustrySchemeConnectorSpec
    extends AnyWordSpec
    with Matchers
    with ScalaFutures
    with IntegrationPatience
    with ApplicationWithWiremock {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val connector: ConstructionIndustrySchemeConnector =
    app.injector.instanceOf[ConstructionIndustrySchemeConnector]

  "getCisTaxpayer" should {

    "return CisTaxpayer when BE returns 200 with valid JSON" in {
      stubFor(
        get(urlPathEqualTo("/cis/taxpayer"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(
                """{
                  |  "uniqueId": "123",
                  |  "taxOfficeNumber": "111",
                  |  "taxOfficeRef": "test111",
                  |  "employerName1": "TEST LTD"
                  |}""".stripMargin
              )
          )
      )

      val result = connector.getCisTaxpayer().futureValue
      result.uniqueId mustBe "123"
      result.taxOfficeNumber mustBe "111"
      result.taxOfficeRef mustBe "test111"
      result.employerName1 mustBe Some("TEST LTD")
    }

    "fail when BE returns 200 with invalid JSON" in {
      stubFor(
        get(urlPathEqualTo("/cis/taxpayer"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody("""{ "unexpectedField": true }""")
          )
      )

      val ex = intercept[Exception] {
        connector.getCisTaxpayer().futureValue
      }
      ex.getMessage.toLowerCase must include("uniqueid")
    }

    "propagate an upstream error when BE returns 500" in {
      stubFor(
        get(urlPathEqualTo("/cis/taxpayer"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.getCisTaxpayer().futureValue
      }
      ex.getMessage must include("returned 500")
    }
  }

  "createAndUpdateSubcontractor" should {

    val cisId = "200"

    "successfully update subcontractor" in {
      stubFor(
        post(urlPathEqualTo("/cis/subcontractor/create-and-update"))
          .willReturn(aResponse().withStatus(NO_CONTENT))
      )

      val payload: CreateAndUpdateSubcontractorPayload =
        IndividualOrSoleTraderPayload(
          cisId = cisId,
          subcontractorType = Individualorsoletrader
        )

      val result: Unit =
        connector.createAndUpdateSubcontractor(payload).futureValue

      result shouldBe ()
    }

    "propagate upstream error on non-2xx" in {
      stubFor(
        post(urlPathEqualTo("/cis/subcontractor/create-and-update"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val payload: CreateAndUpdateSubcontractorPayload =
        IndividualOrSoleTraderPayload(
          cisId = cisId,
          subcontractorType = Individualorsoletrader
        )

      val ex = intercept[Exception] {
        connector.createAndUpdateSubcontractor(payload).futureValue
      }
      ex.getMessage must include("returned 500")
    }
  }

  "getSubcontractorUTRs" should {

    val cisId = "cis-123"

    "successfully get a subcontractor utr list" in {
      val responseJson =
        """
          |{
          |  "subcontractorUTRs": ["1111111111", "2222222222"]
          |}
          |""".stripMargin

      stubFor(
        get(urlPathEqualTo(s"/cis/subcontractors/utr/$cisId"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(responseJson)
          )
      )

      val result = connector.getSubcontractorUTRs(cisId).futureValue
      result.subcontractorUTRs mustBe Seq("1111111111", "2222222222")
    }

    "propagate upstream error on non-2xx" in {
      stubFor(
        get(urlPathEqualTo(s"/cis/subcontractors/utr/$cisId"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.getSubcontractorUTRs(cisId).futureValue
      }
      ex.getMessage must include("returned 500")
    }
  }

  "getAgentClient" should {

    val userId             = "some-user-id"
    val validJson: JsValue = Json.obj(
      "uniqueId"           -> "1",
      "taxOfficeNumber"    -> "123",
      "taxOfficeReference" -> "AB001"
    )

    "returns Some(Json) if OK" in {
      stubFor(
        get(urlEqualTo(s"/cis/user-cache/agent-client/$userId"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(Json.stringify(validJson))
          )
      )

      val result = connector.getAgentClient(userId).futureValue

      result mustBe Some(validJson)
    }

    "returns None if NOT_FOUND" in {
      stubFor(
        get(urlEqualTo(s"/cis/user-cache/agent-client/$userId"))
          .willReturn(
            aResponse()
              .withStatus(NOT_FOUND)
          )
      )

      val result = connector.getAgentClient(userId).futureValue

      result mustBe None
    }

    "throws HttpException for other codes" in {
      stubFor(
        get(urlEqualTo(s"/cis/user-cache/agent-client/$userId"))
          .willReturn(
            aResponse()
              .withStatus(INTERNAL_SERVER_ERROR)
              .withBody("Something broke")
          )
      )

      val result = connector
        .getAgentClient(userId)
        .failed
        .futureValue

      result mustBe a[HttpException]
      result.getMessage must include("Something broke")
    }

  }

  "startClientList" should {

    "POST /cis/agent/client-list/retrieval/start and return succeeded" in {
      stubFor(
        post(urlPathEqualTo("/cis/agent/client-list/retrieval/start"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody("""{ "result": "succeeded" }""")
          )
      )

      connector.startClientList.futureValue.result mustBe
        ClientListStatus.Succeeded
    }
  }

  "hasClient(taxOfficeNumber, taxOfficeReference)" should {

    "GET /cis/agent/has-client/:ton/:tor and return true when BE returns 200" in {
      stubFor(
        get(urlPathEqualTo("/cis/agent/has-client/163/AB0063"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody("""{ "hasClient": true }""")
          )
      )

      connector.hasClient("163", "AB0063").futureValue mustBe true
    }

    "fail the future when BE returns non-200 (e.g. 403)" in {
      stubFor(
        get(urlPathEqualTo("/cis/agent/has-client/163/AB0063"))
          .willReturn(aResponse().withStatus(FORBIDDEN).withBody("""{"error":"nope"}"""))
      )

      val ex = connector.hasClient("163", "AB0063").failed.futureValue
      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe FORBIDDEN
    }
  }

  "getNewestVerificationBatch" should {

    val instanceId = "cis-123"

    "preserve the F3a verification fields returned by the backend" in {
      val responseJson =
        """
          |{
          |  "scheme": null,
          |  "subcontractors": [
          |    {
          |      "subcontractorId": 1,
          |      "subbieResourceRef": 10
          |    }
          |  ],
          |  "verificationBatch": {
          |    "verificationBatchId": 99,
          |    "status": "SUBMITTED",
          |    "verificationNumber": "VB123"
          |  },
          |  "verifications": [
          |    {
          |      "verificationId": 1001,
          |      "matched": "N",
          |      "verificationNumber": "V0000000001",
          |      "taxTreatment": "0",
          |      "verificationBatchId": 99,
          |      "subcontractorId": 1,
          |      "actionIndicator": "VERIFY",
          |      "verificationResourceRef": 10
          |    }
          |  ],
          |  "submission": null,
          |  "monthlyReturn": null,
          |  "monthlyReturnSubmission": null
          |}
          |""".stripMargin

      stubFor(
        get(urlPathEqualTo(s"/cis/verification-batch/newest/$instanceId"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(responseJson)
          )
      )

      val result =
        connector.getNewestVerificationBatch(instanceId).futureValue

      result.verificationBatch.flatMap(_.status) mustBe Some("SUBMITTED")
      result.subcontractors.head.subbieResourceRef mustBe Some(10L)

      result.verifications must have size 1

      val verification = result.verifications.head

      verification.verificationId mustBe 1001L
      verification.verificationNumber mustBe Some("V0000000001")
      verification.subcontractorId mustBe Some(1L)
      verification.actionIndicator mustBe Some("VERIFY")
      verification.matched mustBe Some("N")
      verification.verificationResourceRef mustBe Some(10L)
    }
  }

  "getCurrentVerificationBatch" should {

    val instanceId = "cis-123"

    "successfully get a current verification batch" in {
      val responseJson =
        """
          |{
          |  "subcontractors": [
          |    {
          |      "subcontractorId": 1
          |    }
          |  ],
          |  "verificationBatch": {
          |      "verificationBatchId": 99
          |    },
          |  "verifications": [
          |    {
          |      "verificationId": 1001
          |    }
          |  ]
          |}
          |""".stripMargin

      stubFor(
        get(urlPathEqualTo(s"/cis/verification-batch/current/$instanceId"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withBody(responseJson)
          )
      )

      val result = connector.getCurrentVerificationBatch(instanceId).futureValue
      result.subcontractors.map(_.subcontractorId) mustBe Seq(1L)
      result.verificationBatch.map(_.verificationBatchId) mustBe Some(99L)
      result.verifications.map(_.verificationId) mustBe Seq(1001L)
    }

    "propagate upstream error on non-2xx" in {
      stubFor(
        get(urlPathEqualTo(s"/cis/verification-batch/current/$instanceId"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = intercept[Exception] {
        connector.getCurrentVerificationBatch(instanceId).futureValue
      }
      ex.getMessage must include("returned 500")
    }
  }

  "createVerificationBatchAndVerifications" should {

    "successfully create verification batch and verifications when BE returns 200" in {
      val instanceId = "inst-123"

      val requestModel =
        CreateVerificationBatchAndVerificationsRequest(
          instanceId = instanceId,
          verificationResourceReferences = Seq(111L, 222L, 333L),
          actionIndicator = Some("A")
        )

      val responseJson =
        """
          |{
          |  "verificationBatchResourceReference": 666
          |}
          |""".stripMargin

      stubFor(
        post(urlPathEqualTo("/cis/verification-batch/create"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(responseJson)
          )
      )

      val result = connector.createVerificationBatchAndVerifications(requestModel).futureValue

      result mustBe CreateVerificationBatchAndVerificationsResponse(verificationBatchResourceReference = 666)
    }

    "propagate upstream error on non-2xx (e.g. 500)" in {
      val requestModel =
        CreateVerificationBatchAndVerificationsRequest(
          instanceId = "inst-123",
          verificationResourceReferences = Seq(111L),
          actionIndicator = None
        )

      stubFor(
        post(urlPathEqualTo("/cis/verification-batch/create"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = connector.createVerificationBatchAndVerifications(requestModel).failed.futureValue
      ex.getMessage must include("returned 500")
    }
  }

  "getScheme" should {

    "successfully get scheme when BE returns 200" in {
      val responseJson =
        """
          |{
          |  "schemeId": 1,
          |  "instanceId": "1",
          |  "accountsOfficeReference": "AO123",
          |  "taxOfficeNumber": "123",
          |  "taxOfficeReference": "AB456",
          |  "utr": "1234567890",
          |  "name": "Test Contractor",
          |  "emailAddress": "test@test.com"
          |}
          |""".stripMargin

      stubFor(
        get(urlPathEqualTo("/cis/scheme/1"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(responseJson)
          )
      )

      val result = connector.getScheme("1").futureValue

      result.instanceId mustBe "1"
      result.accountsOfficeReference mustBe "AO123"
      result.utr mustBe Some("1234567890")
    }

    "propagate upstream error on non-2xx" in {
      stubFor(
        get(urlPathEqualTo("/cis/scheme/1"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = connector.getScheme("1").failed.futureValue
      ex.getMessage must include("returned 500")
    }
  }

  "submitVerificationToChris" should {

    "successfully submit verification to ChRIS when BE returns 200" in {
      val requestModel = ChrisVerificationRequest(
        instanceId = "1",
        isAgent = false,
        clientTaxOfficeNumber = "123",
        clientTaxOfficeRef = "AB456",
        contractorUTR = "1234567890",
        contractorAORef = "AO123",
        verificationBatchId = "1001",
        verificationBatchResourceRef = "2001",
        emailRecipient = Some("test@test.com"),
        language = None,
        subcontractors = Seq.empty,
        verifications = Seq(
          VerificationDetails(
            subcontractorName = "Test Subcontractor",
            verificationResourceRef = "4001",
            proceedVerification = true
          )
        )
      )

      val responseJson =
        """
          |{
          |  "submissionId": "13602",
          |  "status": "ACCEPTED",
          |  "hmrcMarkGenerated": "hmrc-mark",
          |  "correlationId": "corr-id",
          |  "responseEndPoint": {
          |    "url": "http://localhost/poll",
          |    "pollIntervalSeconds": 5
          |  },
          |  "gatewayTimestamp": "2026-06-15T03:30:52",
          |  "acceptedTime": "2026-06-15T03:30:53"
          |}
          |""".stripMargin

      stubFor(
        post(urlPathEqualTo("/cis/submissions/13602/submit-verification-to-chris"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(responseJson)
          )
      )

      val result =
        connector.submitVerificationToChris(13602L, requestModel).futureValue

      result.submissionId mustBe "13602"
      result.status mustBe "ACCEPTED"
      result.hmrcMarkGenerated mustBe "hmrc-mark"
      result.responseEndPoint.map(_.pollIntervalSeconds) mustBe Some(5)
    }

    "propagate upstream error on non-2xx" in {
      val requestModel = ChrisVerificationRequest(
        instanceId = "1",
        isAgent = false,
        clientTaxOfficeNumber = "123",
        clientTaxOfficeRef = "AB456",
        contractorUTR = "1234567890",
        contractorAORef = "AO123",
        verificationBatchId = "1001",
        verificationBatchResourceRef = "2001",
        emailRecipient = None,
        language = None,
        subcontractors = Seq.empty,
        verifications = Seq.empty
      )

      stubFor(
        post(urlPathEqualTo("/cis/submissions/13602/submit-verification-to-chris"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex =
        connector.submitVerificationToChris(13602L, requestModel).failed.futureValue

      ex.getMessage must include("returned 500")
    }
  }

  "getSubmissionStatus" should {

    "successfully get verification submission status when BE returns 200" in {
      val responseJson =
        """
          |{
          |  "status": "SUBMITTED",
          |  "correlationId": "corr-id",
          |  "pollUrl": null,
          |  "pollInterval": null,
          |  "error": null,
          |  "irMarkReceived": "ggis-mark",
          |  "lastMessageDate": "2026-06-15T03:30:54",
          |  "acceptedTime": "2026-06-15T03:30:55",
          |  "govTalkErrorStatus": null
          |}
          |""".stripMargin

      stubFor(
        get(urlPathEqualTo("/cis/submissions/verification/poll"))
          .withQueryParam("submissionId", equalTo("13602"))
          .withQueryParam("pollUrl", equalTo("http://localhost/poll"))
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(responseJson)
          )
      )

      val result =
        connector.getSubmissionStatus("http://localhost/poll", "13602").futureValue

      result.status mustBe SubmissionStatus.SUBMITTED
      result.correlationId mustBe "corr-id"
      result.irMarkReceived mustBe Some("ggis-mark")
    }

    "propagate upstream error on non-2xx" in {
      stubFor(
        get(urlPathEqualTo("/cis/submissions/verification/poll"))
          .withQueryParam("submissionId", equalTo("13602"))
          .withQueryParam("pollUrl", equalTo("http://localhost/poll"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex =
        connector.getSubmissionStatus("http://localhost/poll", "13602").failed.futureValue

      ex.getMessage must include("returned 500")
    }
  }

  "proceedInsufficientVerification" should {

    val request = ProceedVerificationRequest(
      instanceId = "1",
      verificationBatchResourceRef = 10L,
      verificationResourceRef = 9L
    )

    "successfully proceed verification when BE returns 204" in {

      stubFor(
        post(urlPathEqualTo("/cis/verification/proceed-with-insufficient-data")).willReturn(
          aResponse().withStatus(NO_CONTENT)
        )
      )

      connector.proceedInsufficientVerification(request).futureValue mustBe ((): Unit)
    }

    "propagate upstream error on non-2xx (e.g. 500)" in {

      stubFor(
        post(urlPathEqualTo("/cis/verification/proceed-with-insufficient-data"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = connector.proceedInsufficientVerification(request).failed.futureValue
      ex.getMessage must include("returned 500")
    }
  }

  "getFinalValidationJourneyHandoff" should {

    val handoffId = "handoff-id"
    val journeyType = JourneyHandoffTypes.FinalValidation

    val payload =
      FinalValidationHandoffPayload(
        draftId = "draft-id",
        instanceId = "1",
        subcontractorId = 1L,
        subbieResourceRef = 100L,
        field = FinalValidationField.Utr,
        changeTarget = FinalValidationChangeTarget.Utr
      )

    "return the handoff payload when BE returns 200" in {

      stubFor(
        get(
          urlPathEqualTo(
            s"/cis/journey-handoffs/$journeyType/$handoffId"
          )
        ).willReturn(
          aResponse()
            .withStatus(OK)
            .withHeader("Content-Type", "application/json")
            .withBody(Json.toJson(payload).toString)
        )
      )

      connector
        .getFinalValidationJourneyHandoff(
          journeyType,
          handoffId
        )
        .futureValue mustBe Some(payload)
    }

    "return None when BE returns 404" in {

      stubFor(
        get(
          urlPathEqualTo(
            s"/cis/journey-handoffs/$journeyType/$handoffId"
          )
        ).willReturn(
          aResponse()
            .withStatus(NOT_FOUND)
        )
      )

      connector
        .getFinalValidationJourneyHandoff(
          journeyType,
          handoffId
        )
        .futureValue mustBe None
    }

    "propagate upstream error when BE returns 500" in {

      stubFor(
        get(
          urlPathEqualTo(
            s"/cis/journey-handoffs/$journeyType/$handoffId"
          )
        ).willReturn(
          aResponse()
            .withStatus(INTERNAL_SERVER_ERROR)
            .withBody("boom")
        )
      )

      val ex =
        connector
          .getFinalValidationJourneyHandoff(
            journeyType,
            handoffId
          )
          .failed
          .futureValue

      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
    }
  }

  "createFinalValidationDraft" should {

    val createDraftRequest =
      CreateFinalValidationDraftRequest(
        instanceId = "1",
        context = "VerifySubcontractor",
        subcontractors = Seq.empty
      )

    "create the draft when BE returns 201" in {

      val response =
        CreateFinalValidationDraftResponse(
          draftId = "draft-id"
        )

      stubFor(
        post(
          urlPathEqualTo(
            "/cis/final-validation/drafts"
          )
        )
          .withHeader(
            "Content-Type",
            containing("application/json")
          )
          .withRequestBody(
            equalToJson(
              Json.toJson(createDraftRequest).toString
            )
          )
          .willReturn(
            aResponse()
              .withStatus(CREATED)
              .withHeader("Content-Type", "application/json")
              .withBody(Json.toJson(response).toString)
          )
      )

      connector
        .createFinalValidationDraft(createDraftRequest)
        .futureValue mustBe response
    }

    "propagate upstream error when BE returns 500" in {

      stubFor(
        post(
          urlPathEqualTo(
            "/cis/final-validation/drafts"
          )
        )
          .willReturn(
            aResponse()
              .withStatus(INTERNAL_SERVER_ERROR)
              .withBody("boom")
          )
      )

      val ex =
        connector
          .createFinalValidationDraft(createDraftRequest)
          .failed
          .futureValue

      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
    }
  }

  "getFinalValidationDraft" should {

    val instanceId = "1"
    val draftId = "draft-id"

    val draftJson =
      Json.obj(
        "subcontractors" -> Json.arr()
      )

    val draft =
      draftJson.as[FinalValidationDraft]

    "return the draft when BE returns 200" in {

      stubFor(
        get(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId"
          )
        ).willReturn(
          aResponse()
            .withStatus(OK)
            .withHeader("Content-Type", "application/json")
            .withBody(draftJson.toString)
        )
      )

      connector
        .getFinalValidationDraft(
          instanceId,
          draftId
        )
        .futureValue mustBe draft
    }

    "propagate upstream error when BE returns 500" in {

      stubFor(
        get(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId"
          )
        ).willReturn(
          aResponse()
            .withStatus(INTERNAL_SERVER_ERROR)
            .withBody("boom")
        )
      )

      val ex =
        connector
          .getFinalValidationDraft(
            instanceId,
            draftId
          )
          .failed
          .futureValue

      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
    }
  }

  "updateFinalValidationReadiness" should {

    val instanceId = "1"
    val draftId = "draft-id"

    val request =
      UpdateFinalValidationReadinessRequest(
        subcontractorId = 1L,
        issues = Seq(
          FinalValidationDraftIssue(
            fieldKey = FinalValidationField.Utr.key,
            value = Some("1234567890")
          )
        )
      )

    val draftJson =
      Json.obj(
        "subcontractors" -> Json.arr()
      )

    val draft =
      draftJson.as[FinalValidationDraft]

    "update the readiness when BE returns 200" in {

      stubFor(
        put(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/readiness"
          )
        )
          .withHeader(
            "Content-Type",
            containing("application/json")
          )
          .withRequestBody(
            equalToJson(
              Json.toJson(request).toString
            )
          )
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(draftJson.toString)
          )
      )

      connector
        .updateFinalValidationReadiness(
          instanceId,
          draftId,
          request
        )
        .futureValue mustBe draft
    }

    "propagate upstream error when BE returns 500" in {

      stubFor(
        put(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/readiness"
          )
        )
          .willReturn(
            aResponse()
              .withStatus(INTERNAL_SERVER_ERROR)
              .withBody("boom")
          )
      )

      val ex =
        connector
          .updateFinalValidationReadiness(
            instanceId,
            draftId,
            request
          )
          .failed
          .futureValue

      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
    }
  }

  "updateFinalValidationCorrection" should {

    val instanceId = "1"
    val draftId = "draft-id"

    val request =
      Json
        .obj(
          "subcontractorId" -> 1L,
          "changeTarget" -> FinalValidationChangeTarget.Utr.key,
          "patch" -> Json.obj(
            "utr" -> "1234567890"
          )
        )
        .as[UpdateFinalValidationCorrectionRequest]

    val draftJson =
      Json.obj(
        "subcontractors" -> Json.arr()
      )

    val draft =
      draftJson.as[FinalValidationDraft]

    "update the correction when BE returns 200" in {

      stubFor(
        put(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/correction"
          )
        )
          .withHeader(
            "Content-Type",
            containing("application/json")
          )
          .withRequestBody(
            equalToJson(
              Json.toJson(request).toString
            )
          )
          .willReturn(
            aResponse()
              .withStatus(OK)
              .withHeader("Content-Type", "application/json")
              .withBody(draftJson.toString)
          )
      )

      connector
        .updateFinalValidationCorrection(
          instanceId,
          draftId,
          request
        )
        .futureValue mustBe draft
    }

    "propagate upstream error when BE returns 500" in {

      stubFor(
        put(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/correction"
          )
        )
          .willReturn(
            aResponse()
              .withStatus(INTERNAL_SERVER_ERROR)
              .withBody("boom")
          )
      )

      val ex =
        connector
          .updateFinalValidationCorrection(
            instanceId,
            draftId,
            request
          )
          .failed
          .futureValue

      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
    }
  }

  "commitFinalValidationDraft" should {

    val instanceId = "1"
    val draftId = "draft-id"

    "successfully commit when BE returns 204" in {

      stubFor(
        post(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/commit"
          )
        ).willReturn(
          aResponse()
            .withStatus(NO_CONTENT)
        )
      )

      connector
        .commitFinalValidationDraft(
          instanceId,
          draftId
        )
        .futureValue mustBe ((): Unit)
    }

    "successfully commit when BE returns 200" in {

      stubFor(
        post(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/commit"
          )
        ).willReturn(
          aResponse()
            .withStatus(OK)
        )
      )

      connector
        .commitFinalValidationDraft(
          instanceId,
          draftId
        )
        .futureValue mustBe ((): Unit)
    }

    "return an UpstreamErrorResponse when BE returns an unexpected status" in {

      stubFor(
        post(
          urlPathEqualTo(
            s"/cis/final-validation/drafts/$instanceId/$draftId/commit"
          )
        ).willReturn(
          aResponse()
            .withStatus(INTERNAL_SERVER_ERROR)
            .withBody("boom")
        )
      )

      val ex =
        connector
          .commitFinalValidationDraft(
            instanceId,
            draftId
          )
          .failed
          .futureValue

      ex mustBe a[UpstreamErrorResponse]
      ex.asInstanceOf[UpstreamErrorResponse].statusCode mustBe INTERNAL_SERVER_ERROR
    }
  }

  "proceedUnmatchedVerification" should {

    val request = ProceedVerificationRequest(
      instanceId = "1",
      verificationBatchResourceRef = 10L,
      verificationResourceRef = 9L
    )

    "successfully proceed verification when BE returns 204" in {

      stubFor(
        post(urlPathEqualTo("/cis/verification/proceed-with-unmatched-data")).willReturn(
          aResponse().withStatus(NO_CONTENT)
        )
      )

      connector.proceedUnmatchedVerification(request).futureValue mustBe ((): Unit)
    }

    "propagate upstream error on non-2xx (e.g. 500)" in {

      stubFor(
        post(urlPathEqualTo("/cis/verification/proceed-with-unmatched-data"))
          .willReturn(aResponse().withStatus(INTERNAL_SERVER_ERROR).withBody("boom"))
      )

      val ex = connector.proceedUnmatchedVerification(request).failed.futureValue
      ex.getMessage must include("returned 500")
    }
  }
}
