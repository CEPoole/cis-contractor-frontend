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

package controllers.verify

import base.SpecBase
import models.{Submission, UserAnswers, VerificationBatch}
import models.response.GetNewestVerificationBatchResponse
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.verify.NewestVerificationBatchResponsePage
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import queries.CisIdQuery
import services.VerificationService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.Future

class VerificationRequestSubmittedControllerSpec extends SpecBase with MockitoSugar {

  private val cisId = "12345"

  private val verificationBatch =
    VerificationBatch(
      verificationBatchId = 1L,
      status = Some("SUBMITTED"),
      verificationNumber = Some("VB00000001")
    )

  private val submission =
    Submission(
      submissionId = 1L,
      activeObjectId = Some(1L),
      submissionRequestDate = Some(LocalDateTime.now()),
      status = Some("SUBMITTED")
    )

  private val newestBatchResponse =
    GetNewestVerificationBatchResponse(
      scheme = None,
      subcontractors = Seq.empty,
      verificationBatch = Some(verificationBatch),
      verifications = Seq.empty,
      submission = Some(submission),
      monthlyReturn = None,
      monthlyReturnSubmission = None
    )

  "VerificationRequestSubmitted Controller" - {

    "must return OK for a GET" in {

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, cisId)
          .success
          .value
          .set(NewestVerificationBatchResponsePage, newestBatchResponse)
          .success
          .value

      val mockVerificationService =
        mock[VerificationService]

      when(
        mockVerificationService.refreshSubmittedVerificationRequest(
          any[UserAnswers]
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(ua))

      when(mockVerificationService.resetUserAnswers(any[UserAnswers]))
        .thenReturn(Future.successful(()))

      val application =
        applicationBuilder(
          userAnswers = Some(ua),
          additionalBindings = Seq(
            bind[VerificationService].toInstance(mockVerificationService)
          )
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.VerificationRequestSubmittedController.onPageLoad().url
          )

        val result =
          route(application, request).value

        status(result) mustEqual OK
      }
    }

    "must redirect to journey recovery when the submitted status check fails" in {

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, cisId)
          .success
          .value
          .set(NewestVerificationBatchResponsePage, newestBatchResponse)
          .success
          .value

      val mockVerificationService =
        mock[VerificationService]

      when(
        mockVerificationService.refreshSubmittedVerificationRequest(
          any[UserAnswers]
        )(any[HeaderCarrier])
      ).thenReturn(Future.failed(new IllegalStateException("invalid status")))

      val application =
        applicationBuilder(
          userAnswers = Some(ua),
          additionalBindings = Seq(
            bind[VerificationService].toInstance(mockVerificationService)
          )
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.VerificationRequestSubmittedController.onPageLoad().url
          )

        val result =
          route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
