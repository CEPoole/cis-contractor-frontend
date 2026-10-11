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

import play.api.libs.json.{Json, Writes}

case class AddressLookupOptionsModel(
  continueUrl: String,
  serviceHref: Option[String] = None,
  signOutHref: Option[String] = None,
  useNewGovUkServiceNavigation: Option[Boolean] = None,
  phaseFeedbackLink: Option[String] = None,
  accessibilityFooterUrl: Option[String] = None,
  deskProServiceName: Option[String] = None,
  showPhaseBanner: Option[Boolean] = None,
  showBackButtons: Option[Boolean] = None,
  disableTranslations: Option[Boolean] = None,
  includeHMRCBranding: Option[Boolean] = None,
  ukMode: Option[Boolean] = None,
  timeoutConfig: Option[AddressLookupTimeoutConfig] = None,
  selectPageConfig: AddressLookupSelectConfigModel,
  confirmPageConfig: AddressLookupConfirmConfigModel,
  manualAddressEntryConfig: ManualAddressEntryConfig,
  pageHeadingStyle: String
)

object AddressLookupOptionsModel {
  implicit val writes: Writes[AddressLookupOptionsModel] = Json.writes[AddressLookupOptionsModel]
}
