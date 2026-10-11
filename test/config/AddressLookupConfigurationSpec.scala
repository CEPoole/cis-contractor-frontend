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
import models.address.AddressLookupJourneyIdentifier.individualQuestionsAddress
import models.address.MandatoryFieldsConfigModel
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.{Call, RequestHeader}
import play.api.test.FakeRequest

class AddressLookupConfigurationSpec extends SpecBase {

  implicit lazy val messagesApi: MessagesApi = app.injector.instanceOf[MessagesApi]
  implicit val rh: RequestHeader             = FakeRequest()

  private val configBuilder = new AddressLookupConfiguration()

  private val continueRoute = Call("GET", "/subcontractor/about-the-land/land-address-return")

  private val mandatoryFields = MandatoryFieldsConfigModel(
    addressLine1 = Some(true),
    town = Some(true),
    postcode = Some(true)
  )

  "AddressLookupConfiguration.apply" - {

    "must build a version 2 configuration" in {
      val result = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = true,
        mandatoryFieldsConfigModel = mandatoryFields
      )

      result.version mustBe 2
    }

    "must set the continue URL to the configured host plus the continue route" in {
      val result = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = true,
        mandatoryFieldsConfigModel = mandatoryFields
      )

      result.options.continueUrl mustBe applicationConfig.host + continueRoute.url
    }

    "must apply the standard CIS option defaults" in {
      val options = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = false,
        mandatoryFieldsConfigModel = mandatoryFields
      ).options

      options.useNewGovUkServiceNavigation mustBe Some(true)
      options.includeHMRCBranding mustBe Some(false)
      options.disableTranslations mustBe Some(false)
      options.showPhaseBanner mustBe Some(true)
      options.showBackButtons mustBe Some(true)
      options.pageHeadingStyle mustBe "govuk-heading-l"
    }

    "must pass through the mandatory fields config and not show the organisation name" in {
      val manualConfig =
        configBuilder(
          individualQuestionsAddress,
          continueRoute,
          useUkMode = true,
          mandatoryFieldsConfigModel = mandatoryFields
        ).options.manualAddressEntryConfig

      manualConfig.mandatoryFields mustBe mandatoryFields
      manualConfig.showOrganisationName mustBe false
    }

    "must set ukMode from the supplied flag" in {
      configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = true,
        mandatoryFieldsConfigModel = mandatoryFields
      ).options.ukMode mustBe Some(true)
      configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = false,
        mandatoryFieldsConfigModel = mandatoryFields
      ).options.ukMode mustBe Some(false)
    }

    "must populate both English and Welsh labels with the service name" in {
      val labels = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = false,
        mandatoryFieldsConfigModel = mandatoryFields
      ).labels

      labels.en.appLevelLabels.navTitle mustBe messagesApi.preferred(Seq(play.api.i18n.Lang("en")))("service.name")
      labels.cy.appLevelLabels.navTitle mustBe messagesApi.preferred(Seq(play.api.i18n.Lang("cy")))("service.name")
    }

    "must omit the country picker labels when in UK mode and include them otherwise" in {
      configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = true,
        mandatoryFieldsConfigModel = mandatoryFields
      ).labels.en.countryPickerLabels mustBe None
      configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = false,
        mandatoryFieldsConfigModel = mandatoryFields
      ).labels.en.countryPickerLabels mustBe defined
    }

    "must produce a JSON-serialisable configuration" in {
      val result = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = false,
        mandatoryFieldsConfigModel = mandatoryFields
      )

      (Json.toJson(result) \ "version").as[Int] mustBe 2
    }

    "must configure the timeout settings for address lookup" in {
      val timeoutConfig = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = true,
        mandatoryFieldsConfigModel = mandatoryFields
      ).options.timeoutConfig

      timeoutConfig mustBe defined
      timeoutConfig.get.timeoutAmount mustBe applicationConfig.timeout
      timeoutConfig.get.timeoutUrl mustBe applicationConfig.feedbackUrl
      timeoutConfig.get.timeoutKeepAliveUrl mustBe Some(applicationConfig.keepAliveUrl)
    }

    "must set the service href to the configured host plus the service home route" in {
      val result = configBuilder(
        individualQuestionsAddress,
        continueRoute,
        useUkMode = true,
        mandatoryFieldsConfigModel = mandatoryFields
      )

      result.options.serviceHref mustBe Some(
        applicationConfig.host + controllers.routes.ServiceHomeController.onPageLoad().url
      )
    }
  }
}
