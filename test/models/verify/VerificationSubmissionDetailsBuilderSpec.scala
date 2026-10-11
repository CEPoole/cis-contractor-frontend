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

import base.SpecBase
import models.response.{ChrisPollResponse, ChrisSubmissionResponse, ResponseEndPointDto}
import models.verify.SubmissionStatus.{SEND_ERROR, SUBMITTED, TIMED_OUT}
import play.api.libs.json.Json
import java.time.LocalDateTime

class VerificationSubmissionDetailsBuilderSpec extends SpecBase {

  "VerificationSubmissionDetailsBuilder" - {

    "must build from ChrisSubmissionResponse" in {
      val response = ChrisSubmissionResponse(
        submissionId = "13602",
        status = "ACCEPTED",
        hmrcMarkGenerated = "hmrc-mark",
        correlationId = Some("123"),
        responseEndPoint = Some(
          ResponseEndPointDto(
            url = "http://localhost/poll",
            pollIntervalSeconds = 5
          )
        ),
        gatewayTimestamp = Some("2026-06-15T03:30:52")
      )

      val result =
        VerificationSubmissionDetailsBuilder.fromSubmissionResponse(
          response,
          LocalDateTime.parse("2026-06-15T03:30:52")
        )

      result mustBe VerificationSubmissionDetails(
        submissionId = "13602",
        status = "ACCEPTED",
        hmrcMarkGenerated = "hmrc-mark",
        hmrcMarkGgis = None,
        correlationId = Some("123"),
        pollUrl = Some("http://localhost/poll"),
        pollIntervalSeconds = Some(5),
        submittedAt = LocalDateTime.parse("2026-06-15T03:30:52"),
        lastMessageDate = None,
        timedOut = false
      )
    }

    "must update from ChrisPollResponse" in {
      val existing = VerificationSubmissionDetails(
        submissionId = "13602",
        status = "ACCEPTED",
        hmrcMarkGenerated = "hmrc-mark",
        hmrcMarkGgis = Some("old-ggis"),
        correlationId = Some("old-corr"),
        pollUrl = Some("http://localhost/old-poll"),
        pollIntervalSeconds = Some(5),
        submittedAt = LocalDateTime.parse("2026-06-15T03:30:52"),
        lastMessageDate = None,
        timedOut = false
      )

      val response = ChrisPollResponse(
        status = SUBMITTED,
        correlationId = "new-corr",
        pollUrl = Some("http://localhost/new-poll"),
        pollInterval = Some(10),
        error = Some(Json.obj("number" -> "E001")),
        irMarkReceived = Some("new-ggis"),
        lastMessageDate = Some("2026-06-15T03:30:54"),
        acceptedTime = Some("2026-06-15T03:30:55")
      )

      val result =
        VerificationSubmissionDetailsBuilder.updateFromPollResponse(existing, response)

      result.status mustBe "SUBMITTED"
      result.correlationId mustBe Some("new-corr")
      result.pollUrl mustBe Some("http://localhost/new-poll")
      result.pollIntervalSeconds mustBe Some(10)
      result.lastMessageDate mustBe Some(LocalDateTime.parse("2026-06-15T03:30:54"))
      result.hmrcMarkGgis mustBe Some("new-ggis")
      result.submissionId mustBe "13602"
      result.hmrcMarkGenerated mustBe "hmrc-mark"
      result.timedOut mustBe false
    }

    "must set timedOut when poll status is TIMED_OUT" in {
      val result =
        VerificationSubmissionDetailsBuilder.updateFromPollResponse(baseDetails, pollResponse(TIMED_OUT))

      result.status mustBe "TIMED_OUT"
      result.timedOut mustBe true
    }

    "must set timedOut when poll status is SEND_ERROR" in {
      val result =
        VerificationSubmissionDetailsBuilder.updateFromPollResponse(baseDetails, pollResponse(SEND_ERROR))

      result.status mustBe "SEND_ERROR"
      result.timedOut mustBe true
    }

    "must keep timedOut set once it has been set on a previous poll" in {
      val result =
        VerificationSubmissionDetailsBuilder.updateFromPollResponse(
          baseDetails.copy(timedOut = true),
          pollResponse(SUBMITTED)
        )

      result.status mustBe "SUBMITTED"
      result.timedOut mustBe true
    }
  }

  private val baseDetails = VerificationSubmissionDetails(
    submissionId = "13602",
    status = "ACCEPTED",
    hmrcMarkGenerated = "hmrc-mark",
    hmrcMarkGgis = None,
    correlationId = Some("corr-id"),
    pollUrl = Some("http://localhost/poll"),
    pollIntervalSeconds = Some(5),
    submittedAt = LocalDateTime.parse("2026-06-15T03:30:52"),
    lastMessageDate = None,
    timedOut = false
  )

  private def pollResponse(pollStatus: SubmissionStatus): ChrisPollResponse =
    ChrisPollResponse(
      status = pollStatus,
      correlationId = "corr-id",
      pollUrl = None,
      pollInterval = None,
      error = None,
      irMarkReceived = None,
      lastMessageDate = None,
      acceptedTime = None
    )
}
