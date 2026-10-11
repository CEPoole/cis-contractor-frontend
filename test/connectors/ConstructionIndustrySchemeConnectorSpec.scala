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

import models.TypeOfSubcontractor
import models.finalvalidation.FinalValidationDraft
import models.requests.*
import models.requests.CreateAndUpdateSubcontractorPayload.*
import models.response.*
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{verify, when}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.http.Status.{INTERNAL_SERVER_ERROR, NO_CONTENT, OK}
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.client.{HttpClientV2, RequestBuilder}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ConstructionIndustrySchemeConnectorSpec extends AnyWordSpec with Matchers with MockitoSugar with ScalaFutures {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "ConstructionIndustrySchemeConnector.createAndUpdateSubcontractor" should {

    "return Unit when CIS responds with NO_CONTENT (204) for IndividualOrSoleTraderPayload" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val payload: CreateAndUpdateSubcontractorPayload =
        IndividualOrSoleTraderPayload(
          cisId = "10",
          subcontractorType = TypeOfSubcontractor.Individualorsoletrader,
          firstName = Some("Jane"),
          surname = Some("Doe")
        )

      connector.createAndUpdateSubcontractor(payload).futureValue mustBe ()

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(payload.asInstanceOf[IndividualOrSoleTraderPayload])
    }

    "return Unit when CIS responds with NO_CONTENT (204) for PartnershipPayload" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val payload: CreateAndUpdateSubcontractorPayload =
        PartnershipPayload(
          cisId = "11",
          subcontractorType = TypeOfSubcontractor.Partnership,
          utr = Some("1234567890"),
          partnerTradingName = Some("Nominated Partner")
        )

      connector.createAndUpdateSubcontractor(payload).futureValue mustBe ()

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(payload.asInstanceOf[PartnershipPayload])
    }

    "return Unit when CIS responds with NO_CONTENT (204) for CompanyPayload" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val payload: CreateAndUpdateSubcontractorPayload =
        CompanyPayload(
          cisId = "11",
          subcontractorType = TypeOfSubcontractor.Limitedcompany,
          utr = Some("1234567890"),
          tradingName = Some("Company Name")
        )

      connector.createAndUpdateSubcontractor(payload).futureValue mustBe ()

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(payload.asInstanceOf[CompanyPayload])
    }

    "fail when CIS responds with a non-204 status" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any()))
        .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR, "boom")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val payload: CreateAndUpdateSubcontractorPayload =
        IndividualOrSoleTraderPayload(
          cisId = "10",
          subcontractorType = TypeOfSubcontractor.Individualorsoletrader
        )

      val ex = connector.createAndUpdateSubcontractor(payload).failed.futureValue
      ex.getMessage mustBe s"Update subcontractor failed, returned $INTERNAL_SERVER_ERROR"
    }
  }

  "ConstructionIndustrySchemeConnector.getNewestVerificationBatch" should {

    "return GetNewestVerificationBatchResponse when CIS returns a valid response" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")
      when(http.get(any())(any())).thenReturn(rb)

      val expected =
        GetNewestVerificationBatchResponse(
          scheme = None,
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil,
          submission = None,
          monthlyReturn = None,
          monthlyReturnSubmission = None
        )

      when(rb.execute[GetNewestVerificationBatchResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val instanceId = "INST-123"
      val result     = connector.getNewestVerificationBatch(instanceId).futureValue

      result mustBe expected

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])

      verify(http).get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString must include("/cis/verification-batch/newest/INST-123")
    }
  }

  "ConstructionIndustrySchemeConnector.updateSchemeVersion" should {

    "POST /cis/scheme/version-update with the request body and return the new version" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)

      val request  = UpdateSchemeVersionRequest(currentVersion = 3, instanceId = "INST-123")
      val response = UpdateSchemeVersionResponse(newVersion = 4)

      when(rb.execute[UpdateSchemeVersionResponse](any(), any()))
        .thenReturn(Future.successful(response))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      connector.updateSchemeVersion(request).futureValue mustBe response

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(request)
    }
  }

  "ConstructionIndustrySchemeConnector.updateScheme" should {

    val request =
      UpdateSchemeRequest(
        schemeId = 1,
        instanceId = "INST-123",
        taxOfficeNumber = "123",
        taxOfficeReference = "AB456",
        accountsOfficeReference = "AO123",
        prePopCount = 2,
        prePopSuccessful = "Y",
        utr = "1234567890",
        name = "Scheme name",
        emailAddress = "test@example.com",
        version = 4
      )

    "POST /cis/scheme/update with the request body and return Unit for 204" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      connector.updateScheme(request).futureValue mustBe (())

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(request)
    }

    "fail with the response body when CIS responds with a non-200/204 status" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any()))
        .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR, "boom")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val ex = connector.updateScheme(request).failed.futureValue
      ex.getMessage mustBe s"Update scheme failed, returned $INTERNAL_SERVER_ERROR: boom"
    }
  }

  "ConstructionIndustrySchemeConnector.getCurrentVerificationBatch" should {

    "return GetCurrentVerificationBatchResponse when CIS returns a valid response" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")
      when(http.get(any())(any())).thenReturn(rb)

      val expected =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil
        )

      when(rb.execute[GetCurrentVerificationBatchResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val instanceId = "INST-123"
      val result     = connector.getCurrentVerificationBatch(instanceId).futureValue

      result mustBe expected

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])

      verify(http).get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString must include("/cis/verification-batch/current/INST-123")
    }
  }

  "ConstructionIndustrySchemeConnector.getLastSubmittedVerificationBatch" should {

    "return GetLastSubmittedVerificationBatchResponse when CIS returns a valid response" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")
      when(http.get(any())(any())).thenReturn(rb)

      val expected =
        GetLastSubmittedVerificationBatchResponse(
          scheme = None,
          subcontractors = Nil,
          verifications = Nil,
          verificationBatch = None,
          submission = None
        )

      when(rb.execute[GetLastSubmittedVerificationBatchResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val instanceId = "INST-123"
      val result     = connector.getLastSubmittedVerificationBatch(instanceId).futureValue

      result mustBe expected

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])

      verify(http).get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString must include("/cis/verification-batch/last/INST-123")
    }
  }

  "ConstructionIndustrySchemeConnector.getSubcontractorList" should {

    "return GetSubcontractorListResponse when CIS returns a valid response" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")
      when(http.get(any())(any())).thenReturn(rb)

      val expected =
        GetSubcontractorListResponse(
          Seq(SubcontractorListItem(11L, None), SubcontractorListItem(22L, Some(222L)))
        )

      when(rb.execute[GetSubcontractorListResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val result = connector.getSubcontractorList("900063").futureValue

      assert(result == expected)

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])

      verify(http).get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString must include("/cis/subcontractors/900063")
    }
  }

  "return Unit when CIS responds with NO_CONTENT (204) for TrustPayload" in {
    val config = mock[ServicesConfig]
    val http   = mock[HttpClientV2]
    val rb     = mock[RequestBuilder]

    when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

    when(http.post(any())(any())).thenReturn(rb)
    when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
    when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

    val connector = new ConstructionIndustrySchemeConnector(config, http)

    val payload: CreateAndUpdateSubcontractorPayload =
      TrustPayload(
        cisId = "12",
        subcontractorType = TypeOfSubcontractor.Trust,
        trustTradingName = Some("Test Trust"),
        utr = Some("1234567890"),
        worksReferenceNumber = Some("WRN-TRUST")
      )

    connector.createAndUpdateSubcontractor(payload).futureValue mustBe ()

    val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
    verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
    bodyCaptor.getValue mustBe Json.toJson(payload.asInstanceOf[TrustPayload])
  }

  "ConstructionIndustrySchemeConnector.modifyVerificationBatch" should {

    "return Unit when CIS responds with NO_CONTENT (204)" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val req = ModifyVerificationsRequest(
        instanceId = "INST-123",
        deleteVerifications = None,
        createVerifications = None
      )

      connector.modifyVerificationBatch(req).futureValue mustBe ()

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(req)
    }

    "return Unit when CIS responds with OK (200)" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(OK, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val req = ModifyVerificationsRequest(
        instanceId = "INST-123",
        deleteVerifications = None,
        createVerifications = None
      )

      connector.modifyVerificationBatch(req).futureValue mustBe ()
    }

    "fail when CIS responds with a non-200/204 status" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any()))
        .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR, "boom")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val req = ModifyVerificationsRequest(
        instanceId = "INST-123",
        deleteVerifications = None,
        createVerifications = None
      )

      val ex = connector.modifyVerificationBatch(req).failed.futureValue
      ex.getMessage mustBe s"Modify verification batch failed, returned $INTERNAL_SERVER_ERROR"
    }
  }

  "ConstructionIndustrySchemeConnector.updateSubcontractor" should {

    val request =
      UpdateSubcontractorRequest(
        cisId = "INST-123",
        subcontractor = SubcontractorRequest(
          subcontractorId = 123L,
          utr = None,
          pageVisited = None,
          partnerUtr = None,
          crn = None,
          firstName = None,
          nino = None,
          secondName = None,
          surname = None,
          partnershipTradingName = None,
          tradingName = None,
          subcontractorType = None,
          addressLine1 = None,
          addressLine2 = None,
          addressLine3 = None,
          addressLine4 = None,
          country = None,
          postcode = None,
          emailAddress = None,
          phoneNumber = None,
          mobilePhoneNumber = None,
          worksReferenceNumber = None,
          createDate = None,
          lastUpdate = None,
          subbieResourceRef = Some(1001L),
          matched = None,
          autoVerified = None,
          verified = None,
          verificationNumber = None,
          taxTreatment = None,
          verificationDate = None,
          version = None,
          updatedTaxTreatment = None,
          lastMonthlyReturnDate = None,
          pendingVerifications = None
        )
      )

    "return Unit when CIS responds with NO_CONTENT (204)" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      connector.updateSubcontractor(request).futureValue mustBe ()

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(request)
    }

    "return Unit when CIS responds with OK (200)" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(OK, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      connector.updateSubcontractor(request).futureValue mustBe ()
    }

    "fail when CIS responds with a non-200/204 status" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any()))
        .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR, "boom")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val ex = connector.updateSubcontractor(request).failed.futureValue
      ex.getMessage mustBe s"Update subcontractor failed, returned $INTERNAL_SERVER_ERROR: boom"
    }
  }

  "ConstructionIndustrySchemeConnector.deleteVerification" should {

    "POST /cis/verification/delete with the request body and return DeleteVerificationResponse" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)

      val expected = DeleteVerificationResponse(verificationsCounter = Some(2L))

      when(rb.execute[DeleteVerificationResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val req = DeleteVerificationRequest(
        instanceId = "INST-123",
        verificationResourceRef = 111L
      )

      val result = connector.deleteVerification(req).futureValue
      result mustBe expected

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])
      verify(http).post(urlCaptor.capture())(any[HeaderCarrier])
      urlCaptor.getValue.toString must include("/cis/verification/delete")

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(req)
    }
  }

  "ConstructionIndustrySchemeConnector.updateSubcontractorForEdit" should {

    val request =
      UpdateSubcontractorForEditRequest(
        cisId = "INST-123",
        subcontractor = SubcontractorRequest(
          subcontractorId = 123L,
          utr = None,
          pageVisited = None,
          partnerUtr = None,
          crn = None,
          firstName = None,
          nino = None,
          secondName = None,
          surname = None,
          partnershipTradingName = None,
          tradingName = None,
          subcontractorType = None,
          addressLine1 = None,
          addressLine2 = None,
          addressLine3 = None,
          addressLine4 = None,
          country = None,
          postcode = None,
          emailAddress = None,
          phoneNumber = None,
          mobilePhoneNumber = None,
          worksReferenceNumber = None,
          createDate = None,
          lastUpdate = None,
          subbieResourceRef = Some(1001L),
          matched = None,
          autoVerified = None,
          verified = None,
          verificationNumber = None,
          taxTreatment = None,
          verificationDate = None,
          version = None,
          updatedTaxTreatment = None,
          lastMonthlyReturnDate = None,
          pendingVerifications = None
        )
      )

    "return Unit when CIS responds with NO_CONTENT (204)" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme"))
        .thenReturn("http://cis-host")

      when(http.post(any())(any()))
        .thenReturn(rb)

      when(rb.withBody(any)(any(), any(), any()))
        .thenReturn(rb)

      when(rb.execute(any(), any()))
        .thenReturn(
          Future.successful(
            HttpResponse(NO_CONTENT, "")
          )
        )

      val connector =
        new ConstructionIndustrySchemeConnector(
          config,
          http
        )

      connector
        .updateSubcontractorForEdit(request)
        .futureValue mustBe (())

      val bodyCaptor =
        ArgumentCaptor.forClass(classOf[JsValue])

      verify(rb)
        .withBody(bodyCaptor.capture())(
          any(),
          any(),
          any()
        )

      bodyCaptor.getValue mustBe Json.toJson(request)
    }

    "return Unit when CIS responds with OK (200)" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme"))
        .thenReturn("http://cis-host")

      when(http.post(any())(any()))
        .thenReturn(rb)

      when(rb.withBody(any)(any(), any(), any()))
        .thenReturn(rb)

      when(rb.execute(any(), any()))
        .thenReturn(
          Future.successful(
            HttpResponse(OK, "")
          )
        )

      val connector =
        new ConstructionIndustrySchemeConnector(
          config,
          http
        )

      connector
        .updateSubcontractorForEdit(request)
        .futureValue mustBe (())
    }

    "fail when CIS responds with a non-200/204 status" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme"))
        .thenReturn("http://cis-host")

      when(http.post(any())(any()))
        .thenReturn(rb)

      when(rb.withBody(any)(any(), any(), any()))
        .thenReturn(rb)

      when(rb.execute(any(), any()))
        .thenReturn(
          Future.successful(
            HttpResponse(
              INTERNAL_SERVER_ERROR,
              "boom"
            )
          )
        )

      val connector =
        new ConstructionIndustrySchemeConnector(
          config,
          http
        )

      val ex =
        connector
          .updateSubcontractorForEdit(request)
          .failed
          .futureValue

      ex.getMessage mustBe
        s"Update subcontractor for edit failed, returned $INTERNAL_SERVER_ERROR: boom"
    }
  }

  "ConstructionIndustrySchemeConnector.createSubmissionForVerification" should {

    "POST /cis/verification/submission/create with the request body and return CreateSubmissionForVerificationResponse" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme")).thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)

      val expected = CreateSubmissionForVerificationResponse(submissionId = 12345L)

      when(rb.execute[CreateSubmissionForVerificationResponse](any(), any()))
        .thenReturn(Future.successful(expected))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val req = CreateSubmissionForVerificationRequest(
        instanceId = "INST-123",
        verificationBatchId = 99L,
        verificationBatchResourceRef = 7777L,
        emailRecipient = Some("ops@example.com"),
        irMarkGenerated = None,
        verifications = Seq(
          VerificationToUpdate(
            subcontractorName = "Unknown",
            verificationResourceRef = 111L,
            proceedVerification = "Y"
          )
        ),
        agentId = None
      )

      val result = connector.createSubmissionForVerification(req).futureValue
      result mustBe expected

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])
      verify(http).post(urlCaptor.capture())(any[HeaderCarrier])
      urlCaptor.getValue.toString must include("/cis/verification/submission/create")

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.toJson(req)
    }
  }

  "ConstructionIndustrySchemeConnector.getSubcontractor" should {

    val cisId             = "INST-123"
    val subbieResourceRef = 1001L

    "GET the subcontractor using the cisId and subbieResourceRef and return the response" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(
        config.baseUrl("construction-industry-scheme")
      ).thenReturn("http://cis-host")

      when(http.get(any())(any()))
        .thenReturn(rb)

      val expected =
        GetSubcontractorResponse(
          scheme = None,
          subcontractor = None
        )

      when(
        rb.execute[GetSubcontractorResponse](any(), any())
      ).thenReturn(
        Future.successful(expected)
      )

      val connector =
        new ConstructionIndustrySchemeConnector(config, http)

      val result =
        connector
          .getSubcontractor(cisId, subbieResourceRef)
          .futureValue

      result mustBe expected

      val urlCaptor =
        ArgumentCaptor.forClass(classOf[URL])

      verify(http)
        .get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString mustBe
        "http://cis-host/cis/subcontractor/INST-123/1001"

      verify(rb)
        .execute[GetSubcontractorResponse](any(), any())
    }

    "return a response containing no subcontractor" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(
        config.baseUrl("construction-industry-scheme")
      ).thenReturn("http://cis-host")

      when(http.get(any())(any()))
        .thenReturn(rb)

      val expected =
        GetSubcontractorResponse(
          scheme = None,
          subcontractor = None
        )

      when(
        rb.execute[GetSubcontractorResponse](any(), any())
      ).thenReturn(
        Future.successful(expected)
      )

      val connector =
        new ConstructionIndustrySchemeConnector(config, http)

      val result =
        connector
          .getSubcontractor(cisId, subbieResourceRef)
          .futureValue

      result.subcontractor mustBe None
      result.scheme mustBe None
    }

    "propagate a failed HTTP request" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(
        config.baseUrl("construction-industry-scheme")
      ).thenReturn("http://cis-host")

      when(http.get(any())(any()))
        .thenReturn(rb)

      when(
        rb.execute[GetSubcontractorResponse](any(), any())
      ).thenReturn(
        Future.failed(
          new RuntimeException("CIS request failed")
        )
      )

      val connector =
        new ConstructionIndustrySchemeConnector(config, http)

      val exception =
        connector
          .getSubcontractor(cisId, subbieResourceRef)
          .failed
          .futureValue

      exception.getMessage mustBe "CIS request failed"

      val urlCaptor =
        ArgumentCaptor.forClass(classOf[URL])

      verify(http)
        .get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString mustBe
        "http://cis-host/cis/subcontractor/INST-123/1001"
    }

    "include a different cisId and subbieResourceRef in the URL" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(
        config.baseUrl("construction-industry-scheme")
      ).thenReturn("http://cis-host")

      when(http.get(any())(any()))
        .thenReturn(rb)

      val expected =
        GetSubcontractorResponse(
          scheme = None,
          subcontractor = None
        )

      when(
        rb.execute[GetSubcontractorResponse](any(), any())
      ).thenReturn(
        Future.successful(expected)
      )

      val connector =
        new ConstructionIndustrySchemeConnector(config, http)

      connector
        .getSubcontractor(
          cisId = "CIS-999",
          subbieResourceRef = 8888L
        )
        .futureValue

      val urlCaptor =
        ArgumentCaptor.forClass(classOf[URL])

      verify(http)
        .get(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString mustBe
        "http://cis-host/cis/subcontractor/CIS-999/8888"
    }
  }

  "ConstructionIndustrySchemeConnector.submitContractorDetails" should {

    "POST /contractor-details/submit with the request body and return Unit" in {

      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme"))
        .thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)

      when(rb.withBody(any[JsValue]())(any(), any(), any()))
        .thenReturn(rb)

      when(rb.execute[HttpResponse](any(), any())).thenReturn(Future.successful(HttpResponse(NO_CONTENT, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val request =
        UpdateContractorSchemeParams(
          schemeId = 123,
          instanceId = "instanceId",
          accountsOfficeReference = "123PA12345678",
          taxOfficeNumber = "123",
          taxOfficeReference = "45678",
          utr = Some("1234567890"),
          name = Some("ABC Ltd"),
          emailAddress = Some("abc@test.com"),
          version = Some(1)
        )

      val result: Unit =
        connector.updateContractorDetails(request).futureValue

      result mustBe ()

      val urlCaptor: ArgumentCaptor[URL] =
        ArgumentCaptor.forClass(classOf[URL])

      verify(http).post(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString must include(
        "/contractor-details/update"
      )

      val bodyCaptor: ArgumentCaptor[JsValue] =
        ArgumentCaptor.forClass(classOf[JsValue])

      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())

      bodyCaptor.getValue mustBe Json.toJson(request)
    }

    "return success when the API returns OK" in {

      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme"))
        .thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any())(any(), any(), any())).thenReturn(rb)

      when(rb.execute[HttpResponse](any(), any()))
        .thenReturn(
          Future.successful(
            HttpResponse(OK, "")
          )
        )

      val connector =
        new ConstructionIndustrySchemeConnector(config, http)

      val request =
        UpdateContractorSchemeParams(
          schemeId = 123,
          instanceId = "instanceId",
          accountsOfficeReference = "123PA12345678",
          taxOfficeNumber = "123",
          taxOfficeReference = "45678"
        )

      connector
        .updateContractorDetails(request)
        .futureValue mustBe ()
    }

    "throw an exception when the API returns a non-success status" in {

      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("construction-industry-scheme"))
        .thenReturn("http://cis-host")

      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any)(any(), any(), any())).thenReturn(rb)
      when(rb.execute[HttpResponse](any(), any()))
        .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR, "")))

      val connector = new ConstructionIndustrySchemeConnector(config, http)

      val request =
        UpdateContractorSchemeParams(
          schemeId = 123,
          instanceId = "instanceId",
          accountsOfficeReference = "123PA12345678",
          taxOfficeNumber = "123",
          taxOfficeReference = "45678"
        )

      val ex =
        connector
          .updateContractorDetails(request)
          .failed
          .futureValue

      ex mustBe a[RuntimeException]

      ex.getMessage mustEqual
        "Update contractor details failed, returned 500"
    }
  }

  "ConstructionIndustrySchemeConnector.resetFinalValidationSubcontractor" should {

    "PUT the reset request and return the updated Final Validation draft" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(
        config.baseUrl("construction-industry-scheme")
      ).thenReturn("http://cis-host")

      when(http.put(any())(any()))
        .thenReturn(rb)

      val expected =
        Json
          .obj(
            "subcontractors" -> Json.arr(
              Json.obj(
                "subcontractorId"   -> 10903L,
                "subbieResourceRef" -> 7L,
                "baseVersion"       -> 12,
                "subcontractorType" -> "soletrader",
                "displayName"       -> "A Alice",
                "base"              -> Json.obj(
                  "firstName" -> "A",
                  "surname"   -> "Alice",
                  "utr"       -> "1111111111"
                ),
                "proposed"          -> Json.obj(
                  "firstName" -> "A",
                  "surname"   -> "Alice",
                  "utr"       -> "1111111111"
                ),
                "changedTargets"    -> Json.arr(),
                "issues"            -> Json.arr(),
                "readiness"         -> "Incomplete",
                "commitStatus"      -> "Pending"
              )
            )
          )
          .as[FinalValidationDraft]

      when(
        rb.execute[FinalValidationDraft](any(), any())
      ).thenReturn(
        Future.successful(expected)
      )

      val connector =
        new ConstructionIndustrySchemeConnector(
          config,
          http
        )

      val result =
        connector
          .resetFinalValidationSubcontractor(
            instanceId = "INST-123",
            draftId = "draft-123",
            subcontractorId = 10903L
          )
          .futureValue

      result mustBe expected

      val urlCaptor =
        ArgumentCaptor.forClass(classOf[URL])

      verify(http)
        .put(urlCaptor.capture())(any[HeaderCarrier])

      urlCaptor.getValue.toString mustBe
        "http://cis-host/cis/final-validation/drafts/INST-123/draft-123/subcontractors/10903/reset"

      verify(rb)
        .execute[FinalValidationDraft](any(), any())
    }

    "propagate a failed HTTP request" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(
        config.baseUrl("construction-industry-scheme")
      ).thenReturn("http://cis-host")

      when(http.put(any())(any()))
        .thenReturn(rb)

      when(
        rb.execute[FinalValidationDraft](any(), any())
      ).thenReturn(
        Future.failed(
          new RuntimeException("CIS request failed")
        )
      )

      val connector =
        new ConstructionIndustrySchemeConnector(
          config,
          http
        )

      val exception =
        connector
          .resetFinalValidationSubcontractor(
            instanceId = "INST-123",
            draftId = "draft-123",
            subcontractorId = 10903L
          )
          .failed
          .futureValue

      exception.getMessage mustBe "CIS request failed"
    }
  }
}
