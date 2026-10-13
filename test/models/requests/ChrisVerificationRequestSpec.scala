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

import base.SpecBase
import play.api.libs.json.*

class ChrisVerificationRequestSpec extends SpecBase {

  "ChrisVerificationRequest" - {

    "must read from JSON" in {
      val json = Json.obj(
        "instanceId"                   -> "1",
        "isAgent"                      -> false,
        "clientTaxOfficeNumber"        -> "123",
        "clientTaxOfficeRef"           -> "AB456",
        "contractorUTR"                -> "1234567890",
        "contractorAORef"              -> "AO123",
        "verificationBatchId"          -> "1001",
        "verificationBatchResourceRef" -> "2001",
        "emailRecipient"               -> "test@test.com",
        "subcontractors"               -> Json.arr(),
        "verifications"                -> Json.arr(
          Json.obj(
            "subcontractorName"       -> "Test Subcontractor",
            "verificationResourceRef" -> "3001",
            "proceedVerification"     -> true
          )
        )
      )

      val result = json.as[ChrisVerificationRequest]

      result.instanceId mustBe "1"
      result.verificationBatchId mustBe "1001"
      result.verifications.head.subcontractorName mustBe "Test Subcontractor"
    }

    "must write to JSON" in {
      val request = ChrisVerificationRequest(
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
            verificationResourceRef = "3001",
            proceedVerification = true
          )
        )
      )

      val json = Json.toJson(request)

      (json \ "instanceId").as[String] mustBe "1"
      (json \ "verificationBatchId").as[String] mustBe "1001"
      (json \ "verifications" \ 0 \ "proceedVerification").as[Boolean] mustBe true
    }
  }
}
