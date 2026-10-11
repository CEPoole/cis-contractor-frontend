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

import models.response.{ChrisPollResponse, ChrisSubmissionResponse}

import java.time.LocalDateTime
import scala.util.Try

object VerificationSubmissionDetailsBuilder {

  def fromSubmissionResponse(
    response: ChrisSubmissionResponse,
    submittedAt: LocalDateTime
  ): VerificationSubmissionDetails =
    VerificationSubmissionDetails(
      submissionId = response.submissionId,
      status = response.status,
      hmrcMarkGenerated = response.hmrcMarkGenerated,
      hmrcMarkGgis = None,
      correlationId = response.correlationId,
      pollUrl = response.responseEndPoint.map(_.url),
      pollIntervalSeconds = response.responseEndPoint.map(_.pollIntervalSeconds),
      submittedAt = submittedAt,
      lastMessageDate = None,
      timedOut = false
    )

  def updateFromPollResponse(
    existing: VerificationSubmissionDetails,
    response: ChrisPollResponse
  ): VerificationSubmissionDetails =
    existing.copy(
      status = response.status.value,
      correlationId = Some(response.correlationId),
      pollUrl = response.pollUrl.orElse(existing.pollUrl),
      pollIntervalSeconds = response.pollInterval.orElse(existing.pollIntervalSeconds),
      lastMessageDate = response.lastMessageDate
        .flatMap(timestamp => Try(LocalDateTime.parse(timestamp)).toOption)
        .orElse(existing.lastMessageDate),
      hmrcMarkGgis = response.irMarkReceived.orElse(existing.hmrcMarkGgis),
      timedOut = existing.timedOut || (response.status match {
        case SubmissionStatus.TIMED_OUT | SubmissionStatus.SEND_ERROR => true
        case _                                                        => false
      })
    )

}
