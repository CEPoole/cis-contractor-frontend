/*
 * Copyright 2025 HM Revenue & Customs
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

package config

import base.SpecBase

class FrontendAppConfigSpec extends SpecBase {

  "FrontendAppConfig" - {

    "addressLookupFrontendUrl must resolve from the address-lookup-frontend service config" in {
      applicationConfig.addressLookupFrontendUrl mustBe "http://localhost:9028"
    }

    "addressLookupJourneyUrl must point at the v2 init endpoint" in {
      applicationConfig.addressLookupJourneyUrl mustBe "http://localhost:9028/api/v2/init"
    }

    "addressLookupRetrievalUrl must point at the v2 confirmed endpoint with the id query parameter" in {
      applicationConfig.addressLookupRetrievalUrl("abc-123") mustBe "http://localhost:9028/api/v2/confirmed?id=abc-123"
    }

    "cisStubUrl must resolve from the construction-industry-scheme-external-stub service config" in {
      applicationConfig.cisStubUrl mustBe "http://localhost:6997"
    }

    "cisFrontendBaseUrl must resolve from the cis-frontend config" in {
      applicationConfig.cisFrontendBaseUrl mustBe "http://localhost:6993"
    }

    "fileStandardReturnUrl must point to the monthly return journey" in {
      applicationConfig.fileStandardReturnUrl mustBe
        "http://localhost:6993/construction-industry-scheme/monthly-return/file-your-monthly-return"
    }

    "fileNilReturnUrl must point to the nil return journey" in {
      applicationConfig.fileNilReturnUrl mustBe
        "http://localhost:6993/construction-industry-scheme/monthly-return/file-your-nil-return"
    }

    "exitSurveyUrl" - {
      "must be built from feedback-frontend service base URL" in {
        applicationConfig.exitSurveyUrl mustBe "http://localhost:9514/feedback/construction-industry-scheme"
      }
    }

    "getConfString" - {

      "must return the configured value when present" in {
        applicationConfig.getConfString("address-lookup-frontend.host", "default") mustBe "localhost"
      }

      "must return the default when absent" in {
        applicationConfig.getConfString("address-lookup-frontend.missing", "default") mustBe "default"
      }
    }

    "getConfInt" - {

      "must return the configured value when present" in {
        applicationConfig.getConfInt("address-lookup-frontend.port", 0) mustBe 9028
      }

      "must return the default when absent" in {
        applicationConfig.getConfInt("address-lookup-frontend.missing", 7) mustBe 7
      }
    }

    "baseUrl" - {

      "must build protocol://host:port from the service config" in {
        applicationConfig.baseUrl("address-lookup-frontend") mustBe "http://localhost:9028"
      }

      "must throw when the host is not configured" in {
        val ex = intercept[RuntimeException](applicationConfig.baseUrl("not-a-real-service"))
        ex.getMessage mustBe "Could not find config key 'not-a-real-service.host'"
      }
    }
  }
}
