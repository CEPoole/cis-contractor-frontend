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

package models.requests

import play.api.libs.json.{Json, OFormat, OWrites}

case class VerificationToUpdate(
  subcontractorName: String,
  verificationResourceRef: Long,
  proceedVerification: String
)

object VerificationToUpdate {
  given OFormat[VerificationToUpdate] = Json.format[VerificationToUpdate]
}

case class CreateSubmissionForVerificationRequest(
  instanceId: String,
  verificationBatchId: Long,
  verificationBatchResourceRef: Long,
  emailRecipient: Option[String],
  language: Option[String] = None,
  irMarkGenerated: Option[String] = None,
  verifications: Seq[VerificationToUpdate],
  agentId: Option[String] = None
)

object CreateSubmissionForVerificationRequest {

  given OFormat[CreateSubmissionForVerificationRequest] = OFormat(
    Json.reads[CreateSubmissionForVerificationRequest],
    OWrites[CreateSubmissionForVerificationRequest] { request =>

      val baseJson = Json.obj(
        "instanceId"                   -> request.instanceId,
        "verificationBatchId"          -> request.verificationBatchId,
        "verificationBatchResourceRef" -> request.verificationBatchResourceRef,
        "emailRecipient"               -> request.emailRecipient.getOrElse(""),
        "verifications"                -> request.verifications
      )

      val withLanguage =
        request.language.fold(baseJson) { lang =>
          baseJson + ("language" -> Json.toJson(lang))
        }

      val withIrMark =
        request.irMarkGenerated.fold(withLanguage) { irMark =>
          withLanguage + ("irMarkGenerated" -> Json.toJson(irMark))
        }

      request.agentId.fold(withIrMark) { agentId =>
        withIrMark + ("agentId" -> Json.toJson(agentId))
      }
    }
  )
}
