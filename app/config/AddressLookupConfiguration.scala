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

import models.address.*
import play.api.i18n.{Lang, MessagesApi}
import play.api.mvc.{Call, RequestHeader}

import javax.inject.Inject

class AddressLookupConfiguration @Inject() (implicit appConfig: FrontendAppConfig, messages: MessagesApi) {

  def apply(
    journeyId: AddressLookupJourneyIdentifier.Value,
    continueRoute: Call,
    useUkMode: Boolean,
    optName: Option[String] = None,
    mandatoryFieldsConfigModel: MandatoryFieldsConfigModel,
    line1MaxLength: Option[Int] = None,
    line2MaxLength: Option[Int] = None,
    line3MaxLength: Option[Int] = None,
    townMaxLength: Option[Int] = None
  )(implicit rh: RequestHeader): AddressLookupConfigurationModel = {
    val english = Lang("en")
    val welsh   = Lang("cy")

    AddressLookupConfigurationModel(
      version = 2,
      options = AddressLookupOptionsModel(
        continueUrl = appConfig.host + continueRoute.url,
        serviceHref = Some(appConfig.host + controllers.routes.ServiceHomeController.onPageLoad().url),
        signOutHref = Some(appConfig.feedbackUrl),
        useNewGovUkServiceNavigation = Some(true),
        phaseFeedbackLink = Some(appConfig.feedbackUrl),
        deskProServiceName = Some(appConfig.contactFormServiceIdentifier),
        showPhaseBanner = Some(true),
        showBackButtons = Some(true),
        disableTranslations = Some(false),
        includeHMRCBranding = Some(false),
        ukMode = Some(useUkMode),
        timeoutConfig = Some(
          AddressLookupTimeoutConfig(
            timeoutAmount = appConfig.timeout,
            timeoutUrl = appConfig.feedbackUrl,
            timeoutKeepAliveUrl = Some(appConfig.keepAliveUrl)
          )
        ),
        selectPageConfig = AddressLookupSelectConfigModel(
          showSearchAgainLink = Some(false)
        ),
        confirmPageConfig = AddressLookupConfirmConfigModel(
          showChangeLinkcontinueUrl = Some(true),
          showSubHeadingAndInfo = Some(false),
          showSearchAgainLink = Some(false),
          showConfirmChangeText = Some(false)
        ),
        manualAddressEntryConfig = ManualAddressEntryConfig(
          line1MaxLength = line1MaxLength,
          line2MaxLength = line2MaxLength,
          line3MaxLength = line3MaxLength,
          townMaxLength = townMaxLength,
          mandatoryFields = mandatoryFieldsConfigModel,
          maxLengthErrorMessages = MaxLengthErrorMessagesModel.forConfig(
            line1MaxLength = line1MaxLength,
            line2MaxLength = line2MaxLength,
            line3MaxLength = line3MaxLength,
            townMaxLength = townMaxLength
          ),
          showOrganisationName = false
        ),
        pageHeadingStyle = "govuk-heading-l"
      ),
      labels = AddressMessageLanguageModel(
        en = AddressMessagesModel.forJourney(journeyId.toString, english, useUkMode, optName),
        cy = AddressMessagesModel.forJourney(journeyId.toString, welsh, useUkMode, optName)
      )
    )
  }

}
