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

package services.finalvalidation

import base.SpecBase
import connectors.ConstructionIndustrySchemeConnector
import models.finalvalidation.*
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{verify, when}
import org.scalatestplus.mockito.MockitoSugar.mock
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

class FinalValidationDraftServiceSpec extends SpecBase {

  implicit val hc: HeaderCarrier    = HeaderCarrier()
  implicit val ec: ExecutionContext = ExecutionContext.global

  "FinalValidationDraftService" - {

    "create" - {

      "must create a Final Validation draft and return the draft id" in {

        val connector = mock[ConstructionIndustrySchemeConnector]
        val service   = new FinalValidationDraftService(connector)

        val request =
          CreateFinalValidationDraftRequest(
            instanceId = "instance-id",
            context = "VerifySubcontractor",
            subcontractors = Seq.empty
          )

        when(
          connector.createFinalValidationDraft(
            any[CreateFinalValidationDraftRequest]
          )(any[HeaderCarrier])
        ).thenReturn(
          Future.successful(
            CreateFinalValidationDraftResponse(
              draftId = "draft-id"
            )
          )
        )

        service.create(request).futureValue mustBe "draft-id"

        verify(connector).createFinalValidationDraft(
          any[CreateFinalValidationDraftRequest]
        )(any[HeaderCarrier])
      }
    }

    "get" - {

      "must get the Final Validation draft" in {

        val connector = mock[ConstructionIndustrySchemeConnector]
        val service   = new FinalValidationDraftService(connector)
        val draft     = null.asInstanceOf[FinalValidationDraft]

        when(
          connector.getFinalValidationDraft(
            any[String],
            any[String]
          )(any[HeaderCarrier])
        ).thenReturn(Future.successful(draft))

        service
          .get(
            "instance-id",
            "draft-id"
          )
          .futureValue mustBe draft

        verify(connector).getFinalValidationDraft(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      }
    }

    "updateReadiness" - {

      "must update the Final Validation readiness" in {

        val connector = mock[ConstructionIndustrySchemeConnector]
        val service   = new FinalValidationDraftService(connector)
        val draft     = null.asInstanceOf[FinalValidationDraft]

        val issues =
          Seq(
            FinalValidationDraftIssue(
              fieldKey = "utr",
              value = Some("1234567890")
            )
          )

        val requestCaptor =
          ArgumentCaptor.forClass(
            classOf[UpdateFinalValidationReadinessRequest]
          )

        when(
          connector.updateFinalValidationReadiness(
            any[String],
            any[String],
            any[UpdateFinalValidationReadinessRequest]
          )(any[HeaderCarrier])
        ).thenReturn(Future.successful(draft))

        service
          .updateReadiness(
            "instance-id",
            "draft-id",
            1L,
            issues
          )
          .futureValue mustBe draft

        verify(connector).updateFinalValidationReadiness(
          any[String],
          any[String],
          requestCaptor.capture()
        )(any[HeaderCarrier])

        requestCaptor.getValue mustBe
          UpdateFinalValidationReadinessRequest(
            subcontractorId = 1L,
            issues = issues
          )
      }
    }

    "updateCorrection" - {

      "must update the Final Validation correction" in {

        val connector = mock[ConstructionIndustrySchemeConnector]
        val service   = new FinalValidationDraftService(connector)
        val draft     = null.asInstanceOf[FinalValidationDraft]

        val patch =
          FinalValidationSubcontractorPatch(
            tradingName = Some("New Trading Name")
          )

        val correction =
          FinalValidationCorrection(
            subcontractorId = 1L,
            changeTarget = FinalValidationChangeTarget.TradingName,
            patch = patch
          )

        val requestCaptor =
          ArgumentCaptor.forClass(
            classOf[UpdateFinalValidationCorrectionRequest]
          )

        when(
          connector.updateFinalValidationCorrection(
            any[String],
            any[String],
            any[UpdateFinalValidationCorrectionRequest]
          )(any[HeaderCarrier])
        ).thenReturn(Future.successful(draft))

        service
          .updateCorrection(
            "instance-id",
            "draft-id",
            correction
          )
          .futureValue mustBe draft

        verify(connector).updateFinalValidationCorrection(
          any[String],
          any[String],
          requestCaptor.capture()
        )(any[HeaderCarrier])

        requestCaptor.getValue mustBe
          UpdateFinalValidationCorrectionRequest(
            subcontractorId = 1L,
            changeTarget = "tradingName",
            patch = patch
          )
      }
    }

    "commit" - {

      "must commit the Final Validation draft" in {

        val connector = mock[ConstructionIndustrySchemeConnector]
        val service   = new FinalValidationDraftService(connector)

        when(
          connector.commitFinalValidationDraft(
            any[String],
            any[String]
          )(any[HeaderCarrier])
        ).thenReturn(Future.unit)

        service
          .commit(
            "instance-id",
            "draft-id"
          )
          .futureValue mustBe ()

        verify(connector).commitFinalValidationDraft(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      }
    }

    "resetSubcontractor" - {

      "must reset the Final Validation subcontractor" in {

        val connector = mock[ConstructionIndustrySchemeConnector]
        val service   = new FinalValidationDraftService(connector)
        val draft     = null.asInstanceOf[FinalValidationDraft]

        when(
          connector.resetFinalValidationSubcontractor(
            any[String],
            any[String],
            any[Long]
          )(any[HeaderCarrier])
        ).thenReturn(Future.successful(draft))

        service
          .resetSubcontractor(
            instanceId = "instance-id",
            draftId = "draft-id",
            subcontractorId = 1L
          )
          .futureValue mustBe draft

        verify(connector).resetFinalValidationSubcontractor(
          any[String],
          any[String],
          any[Long]
        )(any[HeaderCarrier])
      }
    }
  }
}
