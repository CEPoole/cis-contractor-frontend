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

package views.finalvalidations

import base.SpecBase
import models.finalvalidation.*
import play.api.test.CSRFTokenHelper
import play.api.test.FakeRequest
import views.html.finalvalidations.UpdateSubcontractorDetailsView

class UpdateSubcontractorDetailsViewSpec extends SpecBase {

  "UpdateSubcontractorDetailsView" - {

    "must render the subcontractor details and actions" in {

      val view =
        app.injector.instanceOf[UpdateSubcontractorDetailsView]

      val model =
        UpdateSubcontractorDetailsPageModel(
          subcontractorId = 1L,
          subcontractorName = "John Smith",
          rows = Seq(
            UpdateSubcontractorDetailsRow(
              field = FinalValidationField.Utr,
              labelKey = "finalvalidations.updateSubcontractorDetails.soleTrader.utr",
              value = Some("1234567890"),
              changeUrl = "/change-utr"
            )
          )
        )

      val request =
        CSRFTokenHelper.addCSRFToken(
          FakeRequest()
        )

      val result =
        view(model)(
          request,
          messages(app)
        ).toString

      result must include("John Smith")
      result must include("1234567890")
      result must include("""href="/change-utr"""")

      result must include(
        controllers.finalvalidations.routes.UpdateSubcontractorDetailsController
          .onSubmit(1L)
          .url
      )

      result must include(
        controllers.finalvalidations.routes.UpdateSubcontractorDetailsController
          .onCancel(1L)
          .url
      )

      result must include(
        messages(app)(
          "finalvalidations.updateSubcontractorDetails.cancelChanges"
        )
      )
    }

    "must render an empty value when a row has no value" in {

      val view =
        app.injector.instanceOf[UpdateSubcontractorDetailsView]

      val model =
        UpdateSubcontractorDetailsPageModel(
          subcontractorId = 1L,
          subcontractorName = "John Smith",
          rows = Seq(
            UpdateSubcontractorDetailsRow(
              field = FinalValidationField.Utr,
              labelKey = "finalvalidations.updateSubcontractorDetails.soleTrader.utr",
              value = None,
              changeUrl = "/change-utr"
            )
          )
        )

      val request =
        CSRFTokenHelper.addCSRFToken(
          FakeRequest()
        )

      val result =
        view(model)(
          request,
          messages(app)
        ).toString

      result must include("""class="govuk-summary-list__value"""")
      result must include("""href="/change-utr"""")
    }
  }
}
