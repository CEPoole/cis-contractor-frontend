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

package models.address

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{EitherValues, OptionValues}
import play.api.libs.json.*

class AddressLookupModelsSpec extends AnyFreeSpec with Matchers with EitherValues with OptionValues {

  "AddressLookupConfirmConfigModel" - {

    val completeConfig = AddressLookupConfirmConfigModel(
      showChangeLinkcontinueUrl = Some(true),
      showSubHeadingAndInfo = Some(false),
      showSearchAgainLink = Some(true),
      showConfirmChangeText = Some(false)
    )

    val minimalConfig = AddressLookupConfirmConfigModel()

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[AddressLookupConfirmConfigModel]]
      }

      "must serialize config with all fields" in {
        val json = Json.toJson(completeConfig)

        (json \ "showChangeLinkcontinueUrl").asOpt[Boolean] mustBe Some(true)
        (json \ "showSubHeadingAndInfo").asOpt[Boolean] mustBe Some(false)
        (json \ "showSearchAgainLink").asOpt[Boolean] mustBe Some(true)
        (json \ "showConfirmChangeText").asOpt[Boolean] mustBe Some(false)
      }

      "must serialize config with no fields" in {
        val json = Json.toJson(minimalConfig)

        (json \ "showChangeLinkcontinueUrl").toOption mustBe None
        (json \ "showSubHeadingAndInfo").toOption mustBe None
        (json \ "showSearchAgainLink").toOption mustBe None
        (json \ "showConfirmChangeText").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeConfig)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeConfig.showSubHeadingAndInfo mustBe Some(false)
        completeConfig.showSearchAgainLink mustBe Some(true)
        completeConfig.showConfirmChangeText mustBe Some(false)
      }

      "must create instance with default None values" in {
        minimalConfig.showSubHeadingAndInfo must not be defined
        minimalConfig.showSearchAgainLink   must not be defined
        minimalConfig.showConfirmChangeText must not be defined
      }

      "must support equality" in {
        val config1 = completeConfig
        val config2 = completeConfig.copy()

        config1 mustEqual config2
      }
    }
  }

  val testMaxLengthErrorMessage =
    MaxLengthErrorMessage(
      addressLine1 = "Address line 1 max length error",
      addressLine2 = "Address line 2 max length error",
      addressLine3 = "Address line 3 max length error",
      town = "Town max length error"
    )

  val testMaxLengthErrorMessagesModel =
    MaxLengthErrorMessagesModel(
      en = Some(testMaxLengthErrorMessage),
      cy = Some(testMaxLengthErrorMessage)
    )

  "AddressLookupSelectConfigModel" - {

    val completeConfig = AddressLookupSelectConfigModel(
      showSearchAgainLink = Some(true)
    )

    val minimalConfig = AddressLookupSelectConfigModel()

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[AddressLookupSelectConfigModel]]
      }

      "must serialize config with showSearchAgainLink" in {
        val json = Json.toJson(completeConfig)

        (json \ "showSearchAgainLink").asOpt[Boolean] mustBe Some(true)
      }

      "must serialize config with no fields" in {
        val json = Json.toJson(minimalConfig)

        (json \ "showSearchAgainLink").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeConfig)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with field" in {
        completeConfig.showSearchAgainLink mustBe Some(true)
      }

      "must create instance with default None value" in {
        minimalConfig.showSearchAgainLink must not be defined
      }

      "must support equality" in {
        val config1 = completeConfig
        val config2 = completeConfig.copy()

        config1 mustEqual config2
      }

      "must support copy with modifications" in {
        val modified = completeConfig.copy(showSearchAgainLink = Some(false))

        modified.showSearchAgainLink mustBe Some(false)
      }

      "must not be equal when fields differ" in {
        val config1 = completeConfig
        val config2 = completeConfig.copy(showSearchAgainLink = Some(false))

        config1 must not equal config2
      }
    }
  }

  "AddressLookupOptionsModel" - {

    val selectConfig             = AddressLookupSelectConfigModel(showSearchAgainLink = Some(true))
    val confirmConfig            = AddressLookupConfirmConfigModel(showChangeLinkcontinueUrl = Some(true))
    val manualAddressEntryConfig = ManualAddressEntryConfig(
      line1MaxLength = Some(35),
      line2MaxLength = Some(35),
      line3MaxLength = Some(35),
      townMaxLength = Some(35),
      mandatoryFields = MandatoryFieldsConfigModel(
        addressLine1 = Some(true),
        addressLine2 = Some(true),
        addressLine3 = Some(true),
        town = Some(true),
        postcode = Some(true)
      ),
      maxLengthErrorMessages = testMaxLengthErrorMessagesModel,
      showOrganisationName = false
    )
    val completeOptions          = AddressLookupOptionsModel(
      continueUrl = "/continue",
      serviceHref = Some("/service-home"),
      signOutHref = Some("/sign-out"),
      phaseFeedbackLink = Some("/feedback"),
      accessibilityFooterUrl = Some("/accessibility"),
      deskProServiceName = Some("CIS"),
      showPhaseBanner = Some(true),
      showBackButtons = Some(true),
      disableTranslations = Some(true),
      includeHMRCBranding = Some(true),
      ukMode = Some(true),
      selectPageConfig = selectConfig,
      confirmPageConfig = confirmConfig,
      manualAddressEntryConfig = manualAddressEntryConfig,
      pageHeadingStyle = "govuk-heading-l"
    )

    val minimalOptions = AddressLookupOptionsModel(
      continueUrl = "/continue",
      selectPageConfig = AddressLookupSelectConfigModel(),
      confirmPageConfig = AddressLookupConfirmConfigModel(),
      manualAddressEntryConfig = ManualAddressEntryConfig(
        mandatoryFields = MandatoryFieldsConfigModel(),
        maxLengthErrorMessages = testMaxLengthErrorMessagesModel,
        showOrganisationName = false
      ),
      pageHeadingStyle = "govuk-heading-l"
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[AddressLookupOptionsModel]]
      }

      "must serialize options with all fields" in {
        val json = Json.toJson(completeOptions)

        (json \ "continueUrl").as[String] mustBe "/continue"
        (json \ "serviceHref").asOpt[String] mustBe Some("/service-home")
        (json \ "signOutHref").asOpt[String] mustBe Some("/sign-out")
        (json \ "phaseFeedbackLink").asOpt[String] mustBe Some("/feedback")
        (json \ "accessibilityFooterUrl").asOpt[String] mustBe Some("/accessibility")
        (json \ "deskProServiceName").asOpt[String] mustBe Some("CIS")
        (json \ "showPhaseBanner").asOpt[Boolean] mustBe Some(true)
        (json \ "showBackButtons").asOpt[Boolean] mustBe Some(true)
        (json \ "disableTranslations").asOpt[Boolean] mustBe Some(true)
        (json \ "includeHMRCBranding").asOpt[Boolean] mustBe Some(true)
        (json \ "ukMode").asOpt[Boolean] mustBe Some(true)
      }

      "must serialize options with only required fields" in {
        val json = Json.toJson(minimalOptions)

        (json \ "continueUrl").as[String] mustBe "/continue"
        (json \ "selectPageConfig").toOption mustBe defined
        (json \ "confirmPageConfig").toOption mustBe defined
      }

      "must serialize nested configs" in {
        val json = Json.toJson(completeOptions)

        (json \ "selectPageConfig" \ "showSearchAgainLink").asOpt[Boolean] mustBe Some(true)
        (json \ "confirmPageConfig" \ "showChangeLinkcontinueUrl").asOpt[Boolean] mustBe Some(true)
        (json \ "manualAddressEntryConfig" \ "line1MaxLength").asOpt[Int] mustBe Some(35)

        (json \ "manualAddressEntryConfig" \ "maxLengthErrorMessages" \ "en" \ "addressLine1")
          .asOpt[String] mustBe Some("Address line 1 max length error")
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeOptions)

        json mustBe a[JsObject]
        json.as[JsObject].keys must contain allOf ("continueUrl", "selectPageConfig", "confirmPageConfig")
      }

      "must not include None optional fields" in {
        val json = Json.toJson(minimalOptions)

        (json \ "serviceHref").toOption mustBe None
        (json \ "signOutHref").toOption mustBe None
        (json \ "phaseFeedbackLink").toOption mustBe None
        (json \ "deskProServiceName").toOption mustBe None
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeOptions.continueUrl mustBe "/continue"
        completeOptions.serviceHref mustBe Some("/service-home")
        completeOptions.signOutHref mustBe Some("/sign-out")
        completeOptions.deskProServiceName mustBe Some("CIS")
        completeOptions.ukMode mustBe Some(true)
      }

      "must create instance with only required fields" in {
        minimalOptions.continueUrl mustBe "/continue"
        minimalOptions.serviceHref must not be defined
        minimalOptions.signOutHref must not be defined
        minimalOptions.ukMode      must not be defined
      }

      "must support equality" in {
        val options1 = completeOptions
        val options2 = completeOptions.copy()

        options1 mustEqual options2
      }

      "must support copy with modifications" in {
        val modified = completeOptions.copy(continueUrl = "/new-continue")

        modified.continueUrl mustBe "/new-continue"
        modified.serviceHref mustBe completeOptions.serviceHref
        modified.signOutHref mustBe completeOptions.signOutHref
      }

      "must not be equal when fields differ" in {
        val options1 = completeOptions
        val options2 = completeOptions.copy(continueUrl = "/different")

        options1 must not equal options2
      }
    }
  }

  "AppLevelMessagesModel" - {

    val completeMessages = AppLevelMessagesModel(
      navTitle = "Construction Industry Scheme",
      phaseBannerHtml = Some("This is a new service")
    )

    val minimalMessages = AppLevelMessagesModel(
      navTitle = "Construction Industry Scheme"
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[AppLevelMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeMessages)

        (json \ "navTitle").as[String] mustBe "Construction Industry Scheme"
        (json \ "phaseBannerHtml").asOpt[String] mustBe Some("This is a new service")
      }

      "must serialize messages with only required fields" in {
        val json = Json.toJson(minimalMessages)

        (json \ "navTitle").as[String] mustBe "Construction Industry Scheme"
        (json \ "phaseBannerHtml").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeMessages)

        json mustBe a[JsObject]
        json.as[JsObject].keys must contain("navTitle")
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeMessages.navTitle mustBe "Construction Industry Scheme"
        completeMessages.phaseBannerHtml mustBe Some("This is a new service")
      }

      "must create instance with only required fields" in {
        minimalMessages.navTitle mustBe "Construction Industry Scheme"
        minimalMessages.phaseBannerHtml must not be defined
      }

      "must support equality" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeMessages.copy(navTitle = "New Title")

        modified.navTitle mustBe "New Title"
        modified.phaseBannerHtml mustBe completeMessages.phaseBannerHtml
      }

      "must not be equal when fields differ" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy(navTitle = "Different Title")

        messages1 must not equal messages2
      }
    }
  }

  "LookupPageMessagesModel" - {

    val completeMessages = LookupPageMessagesModel(
      title = Some("Find address"),
      heading = Some("Find address"),
      filterLabel = Some("Building name or number"),
      postcodeLabel = Some("Postcode"),
      submitLabel = Some("Find address"),
      noResultsFoundMessage = Some("No results found"),
      resultLimitExceededMessage = Some("Too many results"),
      manualAddressLinkText = Some("Enter address manually")
    )

    val minimalMessages = LookupPageMessagesModel(
      title = None,
      heading = None,
      filterLabel = None,
      postcodeLabel = None,
      submitLabel = None,
      noResultsFoundMessage = None,
      resultLimitExceededMessage = None,
      manualAddressLinkText = None
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[LookupPageMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeMessages)

        (json \ "title").asOpt[String] mustBe Some("Find address")
        (json \ "heading").asOpt[String] mustBe Some("Find address")
        (json \ "filterLabel").asOpt[String] mustBe Some("Building name or number")
        (json \ "postcodeLabel").asOpt[String] mustBe Some("Postcode")
        (json \ "submitLabel").asOpt[String] mustBe Some("Find address")
        (json \ "noResultsFoundMessage").asOpt[String] mustBe Some("No results found")
        (json \ "resultLimitExceededMessage").asOpt[String] mustBe Some("Too many results")
        (json \ "manualAddressLinkText").asOpt[String] mustBe Some("Enter address manually")
      }

      "must serialize messages with no fields" in {
        val json = Json.toJson(minimalMessages)

        (json \ "title").toOption mustBe None
        (json \ "heading").toOption mustBe None
        (json \ "filterLabel").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeMessages)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeMessages.title mustBe Some("Find address")
        completeMessages.heading mustBe Some("Find address")
        completeMessages.submitLabel mustBe Some("Find address")
      }

      "must create instance with no fields" in {
        minimalMessages.title       must not be defined
        minimalMessages.heading     must not be defined
        minimalMessages.submitLabel must not be defined
      }

      "must support equality" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeMessages.copy(title = Some("New Title"))

        modified.title mustBe Some("New Title")
        modified.heading mustBe completeMessages.heading
      }

      "must not be equal when fields differ" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy(title = Some("Different"))

        messages1 must not equal messages2
      }
    }
  }

  "CountryPickerMessagesModel" - {

    val completeMessages = CountryPickerMessagesModel(
      title = Some("Select country"),
      heading = Some("Select country"),
      countryLabel = Some("")
    )

    val minimalMessages = CountryPickerMessagesModel(
      title = None,
      heading = None,
      countryLabel = Some("")
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[CountryPickerMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeMessages)

        (json \ "title").asOpt[String] mustBe Some("Select country")
        (json \ "heading").asOpt[String] mustBe Some("Select country")
      }

      "must serialize messages with no fields" in {
        val json = Json.toJson(minimalMessages)

        (json \ "title").toOption mustBe None
        (json \ "heading").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeMessages)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeMessages.title mustBe Some("Select country")
        completeMessages.heading mustBe Some("Select country")
      }

      "must create instance with no fields" in {
        minimalMessages.title   must not be defined
        minimalMessages.heading must not be defined
      }

      "must support equality" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeMessages.copy(title = Some("New Title"))

        modified.title mustBe Some("New Title")
        modified.heading mustBe completeMessages.heading
      }

      "must not be equal when fields differ" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy(title = Some("Different"))

        messages1 must not equal messages2
      }
    }
  }

  "SelectPageMessagesModel" - {

    val completeMessages = SelectPageMessagesModel(
      title = Some("Select address"),
      heading = Some("Select address"),
      headingWithPostcode = Some("Select an address"),
      proposalListLabel = Some("Select an address"),
      submitLabel = Some("Continue"),
      searchAgainLinkText = Some("Search again"),
      editAddressLinkText = Some("Enter address manually")
    )

    val minimalMessages = SelectPageMessagesModel(
      title = None,
      heading = None,
      headingWithPostcode = None,
      proposalListLabel = None,
      submitLabel = None,
      searchAgainLinkText = None,
      editAddressLinkText = None
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[SelectPageMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeMessages)

        (json \ "title").asOpt[String] mustBe Some("Select address")
        (json \ "heading").asOpt[String] mustBe Some("Select address")
        (json \ "headingWithPostcode").asOpt[String] mustBe Some("Select an address")
        (json \ "proposalListLabel").asOpt[String] mustBe Some("Select an address")
        (json \ "submitLabel").asOpt[String] mustBe Some("Continue")
        (json \ "searchAgainLinkText").asOpt[String] mustBe Some("Search again")
        (json \ "editAddressLinkText").asOpt[String] mustBe Some("Enter address manually")
      }

      "must serialize messages with no fields" in {
        val json = Json.toJson(minimalMessages)

        (json \ "title").toOption mustBe None
        (json \ "heading").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeMessages)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeMessages.title mustBe Some("Select address")
        completeMessages.submitLabel mustBe Some("Continue")
      }

      "must create instance with no fields" in {
        minimalMessages.title       must not be defined
        minimalMessages.submitLabel must not be defined
      }

      "must support equality" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeMessages.copy(title = Some("New Title"))

        modified.title mustBe Some("New Title")
        modified.heading mustBe completeMessages.heading
      }

      "must not be equal when fields differ" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy(title = Some("Different"))

        messages1 must not equal messages2
      }
    }
  }

  "EditPageMessagesModel" - {

    val completeMessages = EditPageMessagesModel(
      title = Some("Edit address"),
      heading = Some("Edit address"),
      line1Label = Some("Address line 1"),
      line2Label = Some("Address line 2"),
      townLabel = Some("Town or city"),
      line3Label = Some("Address line 3"),
      postcodeLabel = Some("Postcode"),
      countryLabel = Some("Country"),
      submitLabel = Some("Continue")
    )

    val minimalMessages = EditPageMessagesModel(
      title = None,
      heading = None,
      line1Label = None,
      line2Label = None,
      townLabel = None,
      line3Label = None,
      postcodeLabel = None,
      countryLabel = None,
      submitLabel = None
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[EditPageMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeMessages)

        (json \ "title").asOpt[String] mustBe Some("Edit address")
        (json \ "heading").asOpt[String] mustBe Some("Edit address")
        (json \ "line1Label").asOpt[String] mustBe Some("Address line 1")
        (json \ "line2Label").asOpt[String] mustBe Some("Address line 2")
        (json \ "townLabel").asOpt[String] mustBe Some("Town or city")
        (json \ "line3Label").asOpt[String] mustBe Some("Address line 3")
        (json \ "postcodeLabel").asOpt[String] mustBe Some("Postcode")
        (json \ "countryLabel").asOpt[String] mustBe Some("Country")
        (json \ "submitLabel").asOpt[String] mustBe Some("Continue")
      }

      "must serialize messages with no fields" in {
        val json = Json.toJson(minimalMessages)

        (json \ "title").toOption mustBe None
        (json \ "heading").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeMessages)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeMessages.title mustBe Some("Edit address")
        completeMessages.line1Label mustBe Some("Address line 1")
        completeMessages.submitLabel mustBe Some("Continue")
      }

      "must create instance with no fields" in {
        minimalMessages.title      must not be defined
        minimalMessages.line1Label must not be defined
      }

      "must support equality" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeMessages.copy(title = Some("New Title"))

        modified.title mustBe Some("New Title")
        modified.heading mustBe completeMessages.heading
      }

      "must not be equal when fields differ" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy(title = Some("Different"))

        messages1 must not equal messages2
      }
    }
  }

  "ConfirmPageMessagesModel" - {

    val completeMessages = ConfirmPageMessagesModel(
      title = Some("Confirm address"),
      heading = Some("Confirm address"),
      infoSubheading = Some("Your selected address"),
      infoMessage = Some("This is your address"),
      submitLabel = Some("Confirm and continue"),
      searchAgainLinkText = Some("Search again"),
      changeLinkText = Some("Change"),
      confirmChangeText = Some("Confirm change")
    )

    val minimalMessages = ConfirmPageMessagesModel(
      title = None,
      heading = None,
      infoSubheading = None,
      infoMessage = None,
      submitLabel = None,
      searchAgainLinkText = None,
      changeLinkText = None,
      confirmChangeText = None
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[ConfirmPageMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeMessages)

        (json \ "title").asOpt[String] mustBe Some("Confirm address")
        (json \ "heading").asOpt[String] mustBe Some("Confirm address")
        (json \ "infoSubheading").asOpt[String] mustBe Some("Your selected address")
        (json \ "infoMessage").asOpt[String] mustBe Some("This is your address")
        (json \ "submitLabel").asOpt[String] mustBe Some("Confirm and continue")
        (json \ "searchAgainLinkText").asOpt[String] mustBe Some("Search again")
        (json \ "changeLinkText").asOpt[String] mustBe Some("Change")
        (json \ "confirmChangeText").asOpt[String] mustBe Some("Confirm change")
      }

      "must serialize messages with no fields" in {
        val json = Json.toJson(minimalMessages)

        (json \ "title").toOption mustBe None
        (json \ "heading").toOption mustBe None
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeMessages)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeMessages.title mustBe Some("Confirm address")
        completeMessages.submitLabel mustBe Some("Confirm and continue")
      }

      "must create instance with no fields" in {
        minimalMessages.title       must not be defined
        minimalMessages.submitLabel must not be defined
      }

      "must support equality" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeMessages.copy(title = Some("New Title"))

        modified.title mustBe Some("New Title")
        modified.heading mustBe completeMessages.heading
      }

      "must not be equal when fields differ" in {
        val messages1 = completeMessages
        val messages2 = completeMessages.copy(title = Some("Different"))

        messages1 must not equal messages2
      }
    }
  }

  "AddressLookupJourneyIdentifier" - {

    "must have individualQuestionsAddress value" in {
      AddressLookupJourneyIdentifier.individualQuestionsAddress mustBe a[AddressLookupJourneyIdentifier.Value]
    }

    "must be an enumeration" in {
      AddressLookupJourneyIdentifier mustBe an[Enumeration]
    }

    "must contain at least one value" in {
      AddressLookupJourneyIdentifier.values must not be empty
    }

    "must have individualQuestionsAddress as a value" in {
      AddressLookupJourneyIdentifier.values must contain(AddressLookupJourneyIdentifier.individualQuestionsAddress)
    }
  }

  "AddressMessageLanguageModel" - {

    val appLevelMessages = AppLevelMessagesModel(
      navTitle = "Construction Industry Scheme",
      phaseBannerHtml = Some("This is a new service")
    )

    val lookupPageMessages = LookupPageMessagesModel(
      title = Some("Find address"),
      heading = Some("Find address"),
      filterLabel = Some("Building name or number"),
      postcodeLabel = Some("Postcode"),
      submitLabel = Some("Find address"),
      noResultsFoundMessage = Some("No results found"),
      resultLimitExceededMessage = Some("Too many results"),
      manualAddressLinkText = Some("Enter address manually")
    )

    val selectPageMessages = SelectPageMessagesModel(
      title = Some("Select address"),
      heading = Some("Select address"),
      headingWithPostcode = Some("Select an address"),
      proposalListLabel = Some("Select an address"),
      submitLabel = Some("Continue"),
      searchAgainLinkText = Some("Search again"),
      editAddressLinkText = Some("Enter address manually")
    )

    val editPageMessages = EditPageMessagesModel(
      title = Some("Edit address"),
      heading = Some("Edit address"),
      line1Label = Some("Address line 1"),
      line2Label = Some("Address line 2"),
      townLabel = Some("Town or city"),
      line3Label = Some("Address line 3"),
      postcodeLabel = Some("Postcode"),
      countryLabel = Some("Country"),
      submitLabel = Some("Continue")
    )

    val confirmPageMessages = ConfirmPageMessagesModel(
      title = Some("Confirm address"),
      heading = Some("Confirm address"),
      infoSubheading = Some("Your selected address"),
      infoMessage = Some("This is your address"),
      submitLabel = Some("Confirm and continue"),
      searchAgainLinkText = Some("Search again"),
      changeLinkText = Some("Change"),
      confirmChangeText = Some("Confirm change")
    )

    val countryPickerMessages = CountryPickerMessagesModel(
      title = Some("Select country"),
      heading = Some("Select country"),
      countryLabel = Some("")
    )

    val internationalMessages = InternationalAddressMessagesModel(
      appLevelLabels = appLevelMessages,
      lookupPageLabels = lookupPageMessages,
      selectPageLabels = selectPageMessages,
      editPageLabels = editPageMessages,
      confirmPageLabels = confirmPageMessages
    )

    val addressMessages = AddressMessagesModel(
      appLevelLabels = appLevelMessages,
      lookupPageLabels = lookupPageMessages,
      selectPageLabels = selectPageMessages,
      editPageLabels = editPageMessages,
      confirmPageLabels = confirmPageMessages,
      countryPickerLabels = Some(countryPickerMessages),
      international = internationalMessages
    )

    val completeLanguageModel = AddressMessageLanguageModel(
      en = addressMessages,
      cy = addressMessages
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[AddressMessageLanguageModel]]
      }

      "must serialize language model with en and cy" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en").as[JsObject] mustBe a[JsObject]
        (json \ "cy").as[JsObject] mustBe a[JsObject]
      }

      "must serialize nested appLevelLabels" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
        (json \ "cy" \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
      }

      "must serialize nested lookupPageLabels" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "lookupPageLabels" \ "title").asOpt[String] mustBe Some("Find address")
        (json \ "cy" \ "lookupPageLabels" \ "title").asOpt[String] mustBe Some("Find address")
      }

      "must serialize nested selectPageLabels" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "selectPageLabels" \ "title").asOpt[String] mustBe Some("Select address")
        (json \ "cy" \ "selectPageLabels" \ "title").asOpt[String] mustBe Some("Select address")
      }

      "must serialize nested editPageLabels" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "editPageLabels" \ "title").asOpt[String] mustBe Some("Edit address")
        (json \ "cy" \ "editPageLabels" \ "title").asOpt[String] mustBe Some("Edit address")
      }

      "must serialize nested confirmPageLabels" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "confirmPageLabels" \ "title").asOpt[String] mustBe Some("Confirm address")
        (json \ "cy" \ "confirmPageLabels" \ "title").asOpt[String] mustBe Some("Confirm address")
      }

      "must serialize nested countryPickerLabels" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "countryPickerLabels" \ "title").asOpt[String] mustBe Some("Select country")
        (json \ "cy" \ "countryPickerLabels" \ "title").asOpt[String] mustBe Some("Select country")
      }

      "must serialize nested international messages" in {
        val json = Json.toJson(completeLanguageModel)

        (json \ "en" \ "international" \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
        (json \ "cy" \ "international" \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeLanguageModel)

        json mustBe a[JsObject]
        json.as[JsObject].keys must contain allOf ("en", "cy")
      }
    }

    "case class" - {

      "must create instance with en and cy" in {
        completeLanguageModel.en mustBe addressMessages
        completeLanguageModel.cy mustBe addressMessages
      }

      "must support equality" in {
        val model1 = completeLanguageModel
        val model2 = completeLanguageModel.copy()

        model1 mustEqual model2
      }

      "must support copy with modifications" in {
        val newMessages = addressMessages.copy(
          appLevelLabels = appLevelMessages.copy(navTitle = "New Title")
        )
        val modified    = completeLanguageModel.copy(en = newMessages)

        modified.en.appLevelLabels.navTitle mustBe "New Title"
        modified.cy mustBe completeLanguageModel.cy
      }

      "must not be equal when en differs" in {
        val differentMessages = addressMessages.copy(
          appLevelLabels = appLevelMessages.copy(navTitle = "Different")
        )
        val model1            = completeLanguageModel
        val model2            = completeLanguageModel.copy(en = differentMessages)

        model1 must not equal model2
      }

      "must not be equal when cy differs" in {
        val differentMessages = addressMessages.copy(
          appLevelLabels = appLevelMessages.copy(navTitle = "Different")
        )
        val model1            = completeLanguageModel
        val model2            = completeLanguageModel.copy(cy = differentMessages)

        model1 must not equal model2
      }
    }
  }

  "AddressMessagesModel" - {

    val appLevelMessages = AppLevelMessagesModel(
      navTitle = "Construction Industry Scheme",
      phaseBannerHtml = Some("This is a new service")
    )

    val lookupPageMessages = LookupPageMessagesModel(
      title = Some("Find address"),
      heading = Some("Find address"),
      filterLabel = Some("Building name or number"),
      postcodeLabel = Some("Postcode"),
      submitLabel = Some("Find address"),
      noResultsFoundMessage = Some("No results found"),
      resultLimitExceededMessage = Some("Too many results"),
      manualAddressLinkText = Some("Enter address manually")
    )

    val selectPageMessages = SelectPageMessagesModel(
      title = Some("Select address"),
      heading = Some("Select address"),
      headingWithPostcode = Some("Select an address"),
      proposalListLabel = Some("Select an address"),
      submitLabel = Some("Continue"),
      searchAgainLinkText = Some("Search again"),
      editAddressLinkText = Some("Enter address manually")
    )

    val editPageMessages = EditPageMessagesModel(
      title = Some("Edit address"),
      heading = Some("Edit address"),
      line1Label = Some("Address line 1"),
      line2Label = Some("Address line 2"),
      townLabel = Some("Town or city"),
      line3Label = Some("Address line 3"),
      postcodeLabel = Some("Postcode"),
      countryLabel = Some("Country"),
      submitLabel = Some("Continue")
    )

    val confirmPageMessages = ConfirmPageMessagesModel(
      title = Some("Confirm address"),
      heading = Some("Confirm address"),
      infoSubheading = Some("Your selected address"),
      infoMessage = Some("This is your address"),
      submitLabel = Some("Confirm and continue"),
      searchAgainLinkText = Some("Search again"),
      changeLinkText = Some("Change"),
      confirmChangeText = Some("Confirm change")
    )

    val countryPickerMessages = CountryPickerMessagesModel(
      title = Some("Select country"),
      heading = Some("Select country"),
      countryLabel = Some("")
    )

    val internationalMessages = InternationalAddressMessagesModel(
      appLevelLabels = appLevelMessages,
      lookupPageLabels = lookupPageMessages,
      selectPageLabels = selectPageMessages,
      editPageLabels = editPageMessages,
      confirmPageLabels = confirmPageMessages
    )

    val completeAddressMessages = AddressMessagesModel(
      appLevelLabels = appLevelMessages,
      lookupPageLabels = lookupPageMessages,
      selectPageLabels = selectPageMessages,
      editPageLabels = editPageMessages,
      confirmPageLabels = confirmPageMessages,
      countryPickerLabels = Some(countryPickerMessages),
      international = internationalMessages
    )

    val minimalAddressMessages = AddressMessagesModel(
      appLevelLabels = appLevelMessages,
      lookupPageLabels = lookupPageMessages,
      selectPageLabels = selectPageMessages,
      editPageLabels = editPageMessages,
      confirmPageLabels = confirmPageMessages,
      countryPickerLabels = None,
      international = internationalMessages
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[AddressMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "appLevelLabels").as[JsObject] mustBe a[JsObject]
        (json \ "lookupPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "selectPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "editPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "confirmPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "countryPickerLabels").as[JsObject] mustBe a[JsObject]
        (json \ "international").as[JsObject] mustBe a[JsObject]
      }

      "must serialize messages without countryPickerLabels" in {
        val json = Json.toJson(minimalAddressMessages)

        (json \ "appLevelLabels").as[JsObject] mustBe a[JsObject]
        (json \ "countryPickerLabels").toOption mustBe None
      }

      "must serialize appLevelLabels correctly" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
        (json \ "appLevelLabels" \ "phaseBannerHtml").asOpt[String] mustBe Some("This is a new service")
      }

      "must serialize lookupPageLabels correctly" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "lookupPageLabels" \ "title").asOpt[String] mustBe Some("Find address")
        (json \ "lookupPageLabels" \ "heading").asOpt[String] mustBe Some("Find address")
      }

      "must serialize selectPageLabels correctly" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "selectPageLabels" \ "title").asOpt[String] mustBe Some("Select address")
        (json \ "selectPageLabels" \ "submitLabel").asOpt[String] mustBe Some("Continue")
      }

      "must serialize editPageLabels correctly" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "editPageLabels" \ "title").asOpt[String] mustBe Some("Edit address")
        (json \ "editPageLabels" \ "line1Label").asOpt[String] mustBe Some("Address line 1")
      }

      "must serialize confirmPageLabels correctly" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "confirmPageLabels" \ "title").asOpt[String] mustBe Some("Confirm address")
        (json \ "confirmPageLabels" \ "submitLabel").asOpt[String] mustBe Some("Confirm and continue")
      }

      "must serialize countryPickerLabels when present" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "countryPickerLabels" \ "title").asOpt[String] mustBe Some("Select country")
        (json \ "countryPickerLabels" \ "heading").asOpt[String] mustBe Some("Select country")
      }

      "must serialize international messages correctly" in {
        val json = Json.toJson(completeAddressMessages)

        (json \ "international" \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
        (json \ "international" \ "lookupPageLabels").as[JsObject] mustBe a[JsObject]
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeAddressMessages)

        json mustBe a[JsObject]
        json
          .as[JsObject]
          .keys must contain allOf ("appLevelLabels", "lookupPageLabels", "selectPageLabels", "editPageLabels", "confirmPageLabels", "international")
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeAddressMessages.appLevelLabels mustBe appLevelMessages
        completeAddressMessages.lookupPageLabels mustBe lookupPageMessages
        completeAddressMessages.selectPageLabels mustBe selectPageMessages
        completeAddressMessages.editPageLabels mustBe editPageMessages
        completeAddressMessages.confirmPageLabels mustBe confirmPageMessages
        completeAddressMessages.countryPickerLabels mustBe Some(countryPickerMessages)
        completeAddressMessages.international mustBe internationalMessages
      }

      "must create instance without countryPickerLabels" in {
        minimalAddressMessages.countryPickerLabels must not be defined
        minimalAddressMessages.appLevelLabels mustBe appLevelMessages
      }

      "must support equality" in {
        val messages1 = completeAddressMessages
        val messages2 = completeAddressMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val newAppLevel = appLevelMessages.copy(navTitle = "New Title")
        val modified    = completeAddressMessages.copy(appLevelLabels = newAppLevel)

        modified.appLevelLabels.navTitle mustBe "New Title"
        modified.lookupPageLabels mustBe completeAddressMessages.lookupPageLabels
      }

      "must not be equal when fields differ" in {
        val different = completeAddressMessages.copy(countryPickerLabels = None)

        completeAddressMessages must not equal different
      }
    }
  }

  "InternationalAddressMessagesModel" - {

    val appLevelMessages = AppLevelMessagesModel(
      navTitle = "Construction Industry Scheme",
      phaseBannerHtml = Some("This is a new service")
    )

    val lookupPageMessages = LookupPageMessagesModel(
      title = Some("Find address"),
      heading = Some("Find address"),
      filterLabel = Some("Building name or number"),
      postcodeLabel = Some("Postcode"),
      submitLabel = Some("Find address"),
      noResultsFoundMessage = Some("No results found"),
      resultLimitExceededMessage = Some("Too many results"),
      manualAddressLinkText = Some("Enter address manually")
    )

    val selectPageMessages = SelectPageMessagesModel(
      title = Some("Select address"),
      heading = Some("Select address"),
      headingWithPostcode = Some("Select an address"),
      proposalListLabel = Some("Select an address"),
      submitLabel = Some("Continue"),
      searchAgainLinkText = Some("Search again"),
      editAddressLinkText = Some("Enter address manually")
    )

    val editPageMessages = EditPageMessagesModel(
      title = Some("Edit address"),
      heading = Some("Edit address"),
      line1Label = Some("Address line 1"),
      line2Label = Some("Address line 2"),
      townLabel = Some("Town or city"),
      line3Label = Some("Address line 3"),
      postcodeLabel = Some("Postcode"),
      countryLabel = Some("Country"),
      submitLabel = Some("Continue")
    )

    val confirmPageMessages = ConfirmPageMessagesModel(
      title = Some("Confirm address"),
      heading = Some("Confirm address"),
      infoSubheading = Some("Your selected address"),
      infoMessage = Some("This is your address"),
      submitLabel = Some("Confirm and continue"),
      searchAgainLinkText = Some("Search again"),
      changeLinkText = Some("Change"),
      confirmChangeText = Some("Confirm change")
    )

    val completeInternationalMessages = InternationalAddressMessagesModel(
      appLevelLabels = appLevelMessages,
      lookupPageLabels = lookupPageMessages,
      selectPageLabels = selectPageMessages,
      editPageLabels = editPageMessages,
      confirmPageLabels = confirmPageMessages
    )

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[InternationalAddressMessagesModel]]
      }

      "must serialize international messages with all fields" in {
        val json = Json.toJson(completeInternationalMessages)

        (json \ "appLevelLabels").as[JsObject] mustBe a[JsObject]
        (json \ "lookupPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "selectPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "editPageLabels").as[JsObject] mustBe a[JsObject]
        (json \ "confirmPageLabels").as[JsObject] mustBe a[JsObject]
      }

      "must serialize appLevelLabels correctly" in {
        val json = Json.toJson(completeInternationalMessages)

        (json \ "appLevelLabels" \ "navTitle").as[String] mustBe "Construction Industry Scheme"
      }

      "must serialize lookupPageLabels correctly" in {
        val json = Json.toJson(completeInternationalMessages)

        (json \ "lookupPageLabels" \ "title").asOpt[String] mustBe Some("Find address")
      }

      "must serialize selectPageLabels correctly" in {
        val json = Json.toJson(completeInternationalMessages)

        (json \ "selectPageLabels" \ "title").asOpt[String] mustBe Some("Select address")
      }

      "must serialize editPageLabels correctly" in {
        val json = Json.toJson(completeInternationalMessages)

        (json \ "editPageLabels" \ "title").asOpt[String] mustBe Some("Edit address")
      }

      "must serialize confirmPageLabels correctly" in {
        val json = Json.toJson(completeInternationalMessages)

        (json \ "confirmPageLabels" \ "title").asOpt[String] mustBe Some("Confirm address")
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeInternationalMessages)

        json mustBe a[JsObject]
        json
          .as[JsObject]
          .keys must contain allOf ("appLevelLabels", "lookupPageLabels", "selectPageLabels", "editPageLabels", "confirmPageLabels")
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeInternationalMessages.appLevelLabels mustBe appLevelMessages
        completeInternationalMessages.lookupPageLabels mustBe lookupPageMessages
        completeInternationalMessages.selectPageLabels mustBe selectPageMessages
        completeInternationalMessages.editPageLabels mustBe editPageMessages
        completeInternationalMessages.confirmPageLabels mustBe confirmPageMessages
      }

      "must support equality" in {
        val messages1 = completeInternationalMessages
        val messages2 = completeInternationalMessages.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val newAppLevel = appLevelMessages.copy(navTitle = "New Title")
        val modified    = completeInternationalMessages.copy(appLevelLabels = newAppLevel)

        modified.appLevelLabels.navTitle mustBe "New Title"
        modified.lookupPageLabels mustBe completeInternationalMessages.lookupPageLabels
      }

      "must not be equal when fields differ" in {
        val newAppLevel = appLevelMessages.copy(navTitle = "Different Title")
        val different   = completeInternationalMessages.copy(appLevelLabels = newAppLevel)

        completeInternationalMessages must not equal different
      }
    }
  }

  "MandatoryFieldsConfigModel" - {

    val completeConfig: MandatoryFieldsConfigModel = MandatoryFieldsConfigModel(
      addressLine1 = Some(true),
      addressLine2 = Some(true),
      addressLine3 = Some(true),
      town = Some(true),
      postcode = Some(true)
    )

    val minimalConfig: MandatoryFieldsConfigModel = MandatoryFieldsConfigModel()

    ".writes" - {

      "must be found implicitly" in {
        implicitly[Writes[ConfirmPageMessagesModel]]
      }

      "must serialize messages with all fields" in {
        val json = Json.toJson(completeConfig)

        (json \ "addressLine1").asOpt[Boolean] mustBe Some(true)
        (json \ "addressLine2").asOpt[Boolean] mustBe Some(true)
        (json \ "addressLine3").asOpt[Boolean] mustBe Some(true)
        (json \ "town").asOpt[Boolean] mustBe Some(true)
        (json \ "postcode").asOpt[Boolean] mustBe Some(true)
      }

      "must produce valid JSON structure" in {
        val json = Json.toJson(completeConfig)

        json mustBe a[JsObject]
      }
    }

    "case class" - {

      "must create instance with all fields" in {
        completeConfig.addressLine1 mustBe Some(true)
        completeConfig.addressLine2 mustBe Some(true)
        completeConfig.addressLine3 mustBe Some(true)
        completeConfig.town mustBe Some(true)
        completeConfig.postcode mustBe Some(true)
      }

      "must create instance with no fields" in {
        minimalConfig.addressLine1 must not be defined
        minimalConfig.addressLine2 must not be defined
        minimalConfig.addressLine3 must not be defined
        minimalConfig.town         must not be defined
        minimalConfig.postcode     must not be defined
      }

      "must support equality" in {
        val messages1 = completeConfig
        val messages2 = completeConfig.copy()

        messages1 mustEqual messages2
      }

      "must support copy with modifications" in {
        val modified = completeConfig.copy(addressLine1 = Some(false))

        modified.addressLine1 mustBe Some(false)
        modified.addressLine2 mustBe completeConfig.addressLine2
      }

      "must not be equal when fields differ" in {
        val messages1 = completeConfig
        val messages2 = completeConfig.copy(addressLine1 = Some(false))

        messages1 must not equal messages2
      }
    }
  }
}
