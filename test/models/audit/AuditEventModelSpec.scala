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

package models.audit

import base.SpecBase
import models.address.{Address, Country}
import play.api.libs.json.{JsObject, JsValue, Json}

class AuditEventModelSpec extends SpecBase {

  "AuthFailureAuditEventModel" - {

    val underTest = AuthFailureAuditEventModel(
      agentUserId = "agent-123",
      taxOfficeNumber = "123",
      taxOfficeReference = "AB456",
      clientUniqueId = "unique-id-789"
    )

    "must serialise correctly" in {
      Json.toJson(underTest) mustBe Json.obj(
        "agentUserId"        -> "agent-123",
        "taxOfficeNumber"    -> "123",
        "taxOfficeReference" -> "AB456",
        "clientUniqueId"     -> "unique-id-789"
      )
    }
  }

  "extendedDataEvent" - {

    val testAuditType: String   = "test-audit-type"
    val testDetailJson: JsValue = Json.toJson(testAuditType)

    "behave as expected" in {
      val event    = new AuditEventModel {
        override val auditType: String   = testAuditType
        override val detailJson: JsValue = testDetailJson
      }
      val extended = event.extendedDataEvent

      extended.auditType mustBe testAuditType
      extended.detail mustBe testDetailJson
    }
  }

  private val address = Address(
    addressLine1 = "4 Other Place",
    addressLine2 = Some("Some District"),
    addressLine3 = Some("Anytown"),
    postcode = Some("ZZ1 1ZZ"),
    country = Some(Country(Some("GB"), Some("United Kingdom"))),
    addressValidated = true
  )

  "AddSubcontractorAuditEventModel" - {

    "must serialise with only the mandatory field when all optionals are absent" in {
      val model = AddSubcontractorAuditEventModel(
        cisId = None,
        typeOfSubcontractor = "soletrader",
        subcontractorNameSelected = None,
        tradingNameSelected = None,
        firstName = None,
        middleName = None,
        surname = None,
        tradingNameOfSubcontractor = None,
        subAddressYesNo = None,
        addressOfSubcontractor = None,
        addIndividualContactMethodsYesNo = None,
        individualEmailContactMethod = None,
        individualPhoneContactMethod = None,
        individualMobileContactMethod = None,
        individualEmailAddress = None,
        individualPhoneNumber = None,
        individualMobileNumber = None,
        uniqueTaxpayerReferenceYesNo = None,
        subcontractorsUniqueTaxpayerReference = None,
        nationalInsuranceNumberYesNo = None,
        subNationalInsuranceNumber = None,
        worksReferenceNumberYesNo = None,
        worksReferenceNumber = None
      )
      Json.toJson(model) mustEqual Json.obj("typeOfSubcontractor" -> "soletrader")
    }

    "must serialise all fields when all are present" in {
      val model = AddSubcontractorAuditEventModel(
        cisId = Some("1"),
        typeOfSubcontractor = "soletrader",
        subcontractorNameSelected = Some(true),
        tradingNameSelected = Some(true),
        firstName = Some("John"),
        middleName = Some("Paul"),
        surname = Some("Smith"),
        tradingNameOfSubcontractor = Some("TradingName"),
        subAddressYesNo = Some(true),
        addressOfSubcontractor = Some(address),
        addIndividualContactMethodsYesNo = Some(true),
        individualEmailContactMethod = Some(true),
        individualPhoneContactMethod = Some(true),
        individualMobileContactMethod = Some(true),
        individualEmailAddress = Some("test@test.com"),
        individualPhoneNumber = Some("+447960141611"),
        individualMobileNumber = Some("01912170507"),
        uniqueTaxpayerReferenceYesNo = Some(true),
        subcontractorsUniqueTaxpayerReference = Some("1111122222"),
        nationalInsuranceNumberYesNo = Some(true),
        subNationalInsuranceNumber = Some("NH112233D"),
        worksReferenceNumberYesNo = Some(true),
        worksReferenceNumber = Some("WORKREF-001")
      )
      Json.toJson(model) mustEqual Json.obj(
        "cisId"                                 -> "1",
        "typeOfSubcontractor"                   -> "soletrader",
        "subcontractorNameSelected"             -> true,
        "tradingNameSelected"                   -> true,
        "firstName"                             -> "John",
        "middleName"                            -> "Paul",
        "surname"                               -> "Smith",
        "tradingNameOfSubcontractor"            -> "TradingName",
        "subcontractorAddressYesNo"             -> true,
        "addressOfSubcontractor"                -> Json.toJson(address)(Address.auditWrites),
        "addIndividualContactMethodsYesNo"      -> true,
        "individualEmailContactMethod"          -> true,
        "individualPhoneContactMethod"          -> true,
        "individualMobileContactMethod"         -> true,
        "individualEmailAddress"                -> "test@test.com",
        "individualPhoneNumber"                 -> "+447960141611",
        "individualMobileNumber"                -> "01912170507",
        "uniqueTaxpayerReferenceYesNo"          -> true,
        "subcontractorsUniqueTaxpayerReference" -> "1111122222",
        "nationalInsuranceNumberYesNo"          -> true,
        "subcontractorNationalInsuranceNumber"  -> "NH112233D",
        "worksReferenceNumberYesNo"             -> true,
        "worksReferenceNumber"                  -> "WORKREF-001"
      )
    }

    "must have auditType AddSubcontractor" in {
      AddSubcontractorAuditEventModel(
        cisId = None,
        typeOfSubcontractor = "soletrader",
        subcontractorNameSelected = None,
        tradingNameSelected = None,
        firstName = None,
        middleName = None,
        surname = None,
        tradingNameOfSubcontractor = None,
        subAddressYesNo = None,
        addressOfSubcontractor = None,
        addIndividualContactMethodsYesNo = None,
        individualEmailContactMethod = None,
        individualPhoneContactMethod = None,
        individualMobileContactMethod = None,
        individualEmailAddress = None,
        individualPhoneNumber = None,
        individualMobileNumber = None,
        uniqueTaxpayerReferenceYesNo = None,
        subcontractorsUniqueTaxpayerReference = None,
        nationalInsuranceNumberYesNo = None,
        subNationalInsuranceNumber = None,
        worksReferenceNumberYesNo = None,
        worksReferenceNumber = None
      ).auditType mustBe "AddSubcontractor"
    }
  }

  "AddCompanySubcontractorAuditEventModel" - {

    "must serialise with only the mandatory field when all optionals are absent" in {
      val model = AddCompanySubcontractorAuditEventModel(
        cisId = None,
        typeOfSubcontractor = "company",
        companyName = None,
        companyAddressYesNo = None,
        companyAddress = None,
        addCompanyContactMethodsYesNo = None,
        companyEmailContactMethod = None,
        companyPhoneContactMethod = None,
        companyMobileContactMethod = None,
        companyEmailAddress = None,
        companyPhoneNumber = None,
        companyMobileNumber = None,
        companyUtrYesNo = None,
        companyUtr = None,
        companyCrnYesNo = None,
        companyCrn = None,
        companyWorksReferenceYesNo = None,
        companyWorksReference = None
      )
      Json.toJson(model) mustEqual Json.obj("typeOfSubcontractor" -> "company")
    }

    "must serialise all fields when all are present" in {
      val model = AddCompanySubcontractorAuditEventModel(
        cisId = Some("1"),
        typeOfSubcontractor = "company",
        companyName = Some("Test Co Ltd"),
        companyAddressYesNo = Some(true),
        companyAddress = Some(address),
        addCompanyContactMethodsYesNo = Some(true),
        companyEmailContactMethod = Some(true),
        companyPhoneContactMethod = Some(false),
        companyMobileContactMethod = Some(false),
        companyEmailAddress = Some("company@test.com"),
        companyPhoneNumber = Some("01912170507"),
        companyMobileNumber = Some("+447960141611"),
        companyUtrYesNo = Some(true),
        companyUtr = Some("1111122222"),
        companyCrnYesNo = Some(true),
        companyCrn = Some("12345678"),
        companyWorksReferenceYesNo = Some(true),
        companyWorksReference = Some("WORKREF-002")
      )
      Json.toJson(model) mustEqual Json.obj(
        "cisId"                         -> "1",
        "typeOfSubcontractor"           -> "company",
        "companyName"                   -> "Test Co Ltd",
        "companyAddressYesNo"           -> true,
        "companyAddress"                -> Json.toJson(address)(Address.auditWrites),
        "addCompanyContactMethodsYesNo" -> true,
        "companyEmailContactMethod"     -> true,
        "companyPhoneContactMethod"     -> false,
        "companyMobileContactMethod"    -> false,
        "companyEmailAddress"           -> "company@test.com",
        "companyPhoneNumber"            -> "01912170507",
        "companyMobileNumber"           -> "+447960141611",
        "companyUtrYesNo"               -> true,
        "companyUtr"                    -> "1111122222",
        "companyCrnYesNo"               -> true,
        "companyCrn"                    -> "12345678",
        "companyWorksReferenceYesNo"    -> true,
        "companyWorksReference"         -> "WORKREF-002"
      )
    }
  }

  "AddPartnershipSubcontractorAuditEventModel" - {

    "must serialise with only the mandatory field when all optionals are absent" in {
      val model = AddPartnershipSubcontractorAuditEventModel(
        cisId = None,
        typeOfSubcontractor = "partnership",
        partnershipName = None,
        partnershipAddressYesNo = None,
        partnershipAddress = None,
        addPartnershipContactMethodsYesNo = None,
        partnershipEmailContactMethod = None,
        partnershipPhoneContactMethod = None,
        partnershipMobileContactMethod = None,
        partnershipEmailAddress = None,
        partnershipPhoneNumber = None,
        partnershipMobileNumber = None,
        partnershipHasUtrYesNo = None,
        partnershipUniqueTaxpayerReference = None,
        partnershipNominatedPartnerName = None,
        partnershipNominatedPartnerUtrYesNo = None,
        partnershipNominatedPartnerUtr = None,
        partnershipNominatedPartnerNinoYesNo = None,
        nominatedPartnerNationalInsuranceNumber = None,
        partnershipNominatedPartnerCrnYesNo = None,
        nominatedPartnerCompanyRegistrationNumber = None,
        partnershipWorksReferenceNumberYesNo = None,
        partnershipWorksReference = None
      )
      Json.toJson(model) mustEqual Json.obj("typeOfSubcontractor" -> "partnership")
    }

    "must serialise all fields when all are present" in {
      val model = AddPartnershipSubcontractorAuditEventModel(
        cisId = Some("1"),
        typeOfSubcontractor = "partnership",
        partnershipName = Some("Test Partnership"),
        partnershipAddressYesNo = Some(true),
        partnershipAddress = Some(address),
        addPartnershipContactMethodsYesNo = Some(true),
        partnershipEmailContactMethod = Some(true),
        partnershipPhoneContactMethod = Some(false),
        partnershipMobileContactMethod = Some(false),
        partnershipEmailAddress = Some("partnership@test.com"),
        partnershipPhoneNumber = Some("01912170507"),
        partnershipMobileNumber = Some("+447960141611"),
        partnershipHasUtrYesNo = Some(true),
        partnershipUniqueTaxpayerReference = Some("1111122222"),
        partnershipNominatedPartnerName = Some("Nominated Partner"),
        partnershipNominatedPartnerUtrYesNo = Some(true),
        partnershipNominatedPartnerUtr = Some("2222233333"),
        partnershipNominatedPartnerNinoYesNo = Some(true),
        nominatedPartnerNationalInsuranceNumber = Some("NH112233D"),
        partnershipNominatedPartnerCrnYesNo = Some(true),
        nominatedPartnerCompanyRegistrationNumber = Some("12345678"),
        partnershipWorksReferenceNumberYesNo = Some(true),
        partnershipWorksReference = Some("WORKREF-003")
      )
      Json.toJson(model) mustEqual Json.obj(
        "cisId"                                     -> "1",
        "typeOfSubcontractor"                       -> "partnership",
        "partnershipName"                           -> "Test Partnership",
        "partnershipAddressYesNo"                   -> true,
        "partnershipAddress"                        -> Json.toJson(address)(Address.auditWrites),
        "addPartnershipContactMethodsYesNo"         -> true,
        "partnershipEmailContactMethod"             -> true,
        "partnershipPhoneContactMethod"             -> false,
        "partnershipMobileContactMethod"            -> false,
        "partnershipEmailAddress"                   -> "partnership@test.com",
        "partnershipPhoneNumber"                    -> "01912170507",
        "partnershipMobileNumber"                   -> "+447960141611",
        "partnershipHasUtrYesNo"                    -> true,
        "partnershipUniqueTaxpayerReference"        -> "1111122222",
        "partnershipNominatedPartnerName"           -> "Nominated Partner",
        "partnershipNominatedPartnerUtrYesNo"       -> true,
        "partnershipNominatedPartnerUtr"            -> "2222233333",
        "partnershipNominatedPartnerNinoYesNo"      -> true,
        "nominatedPartnerNationalInsuranceNumber"   -> "NH112233D",
        "partnershipNominatedPartnerCrnYesNo"       -> true,
        "nominatedPartnerCompanyRegistrationNumber" -> "12345678",
        "partnershipWorksReferenceNumberYesNo"      -> true,
        "partnershipWorksReference"                 -> "WORKREF-003"
      )
    }
  }

  private val baseIndividualDetails = IndividualSubcontractorDetails(
    subcontractorNameSelected = Some(true),
    tradingNameSelected = Some(false),
    firstName = Some("John"),
    middleName = Some("Paul"),
    surname = Some("Smith"),
    tradingNameOfSubcontractor = None,
    subAddressYesNo = Some(true),
    addressOfSubcontractor = Some(address),
    addIndividualContactMethodsYesNo = Some(true),
    individualEmailContactMethod = Some(true),
    individualPhoneContactMethod = Some(true),
    individualMobileContactMethod = Some(true),
    individualEmailAddress = Some("sub@example.com"),
    individualPhoneNumber = Some("01234567890"),
    individualMobileNumber = Some("07123456789"),
    uniqueTaxpayerReferenceYesNo = Some(true),
    subcontractorsUniqueTaxpayerReference = Some("2736707626"),
    nationalInsuranceNumberYesNo = Some(true),
    subNationalInsuranceNumber = Some("AA123456C"),
    worksReferenceNumberYesNo = Some(true),
    worksReferenceNumber = Some("WR-123")
  )

  private val baseCompanyDetails = CompanySubcontractorDetails(
    companyName = Some("Test Co Ltd"),
    companyAddressYesNo = Some(true),
    companyAddress = Some(address),
    addCompanyContactMethodsYesNo = Some(true),
    companyEmailContactMethod = Some(true),
    companyPhoneContactMethod = Some(false),
    companyMobileContactMethod = Some(false),
    companyEmailAddress = Some("company@example.com"),
    companyPhoneNumber = Some("01234567890"),
    companyMobileNumber = Some("07123456789"),
    companyUtrYesNo = Some(true),
    companyUtr = Some("1111122222"),
    companyCrnYesNo = Some(true),
    companyCrn = Some("12345678"),
    companyWorksReferenceYesNo = Some(true),
    companyWorksReference = Some("WR-456")
  )

  private val basePartnershipDetails = PartnershipSubcontractorDetails(
    partnershipName = Some("Test Partnership"),
    partnershipAddressYesNo = Some(true),
    partnershipAddress = Some(address),
    addPartnershipContactMethodsYesNo = Some(true),
    partnershipEmailContactMethod = Some(true),
    partnershipPhoneContactMethod = Some(false),
    partnershipMobileContactMethod = Some(false),
    partnershipEmailAddress = Some("partnership@example.com"),
    partnershipPhoneNumber = Some("01234567890"),
    partnershipMobileNumber = Some("07123456789"),
    partnershipHasUtrYesNo = Some(true),
    partnershipUniqueTaxpayerReference = Some("1111122222"),
    partnershipNominatedPartnerName = Some("Nominated Partner"),
    partnershipNominatedPartnerUtrYesNo = Some(true),
    partnershipNominatedPartnerUtr = Some("2222233333"),
    partnershipNominatedPartnerNinoYesNo = Some(true),
    nominatedPartnerNationalInsuranceNumber = Some("NH112233D"),
    partnershipNominatedPartnerCrnYesNo = Some(true),
    nominatedPartnerCompanyRegistrationNumber = Some("12345678"),
    partnershipWorksReferenceNumberYesNo = Some(true),
    partnershipWorksReference = Some("WR-789")
  )

  private val baseTrustDetails = TrustSubcontractorDetails(
    trustName = Some("Test Trust"),
    trustAddressYesNo = Some(true),
    trustAddress = Some(address),
    addTrustContactMethodsYesNo = Some(true),
    trustEmailContactMethod = Some(true),
    trustPhoneContactMethod = Some(false),
    trustMobileContactMethod = Some(false),
    trustEmailAddress = Some("trust@example.com"),
    trustPhoneNumber = Some("01234567890"),
    trustMobileNumber = Some("07123456789"),
    trustUtrYesNo = Some(true),
    trustUtr = Some("1111122222"),
    trustWorksReferenceYesNo = Some(true),
    trustWorksReference = Some("WR-012")
  )

  "AmendSubcontractorAuditEventModel" - {

    "must serialise updatedDetails in full" in {
      val model = AmendSubcontractorAuditEventModel(
        cisId = Some("1"),
        subbieResourceRef = None,
        typeOfSubcontractor = "soletrader",
        updatedDetails = baseIndividualDetails
      )
      val json  = Json.toJson(model)
      (json \ "updatedDetails").as[JsObject] mustEqual Json.toJson(baseIndividualDetails).as[JsObject]
      (json \ "originalDetails").toOption mustBe None
    }

    "must omit cisId when absent" in {
      val model = AmendSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "soletrader",
        updatedDetails = baseIndividualDetails
      )
      (Json.toJson(model) \ "cisId").toOption mustBe None
    }

    "must include subcontractorResourceRef in JSON when present" in {
      val model = AmendSubcontractorAuditEventModel(
        cisId = Some("1"),
        subbieResourceRef = Some(42L),
        typeOfSubcontractor = "soletrader",
        updatedDetails = baseIndividualDetails
      )
      (Json.toJson(model) \ "subcontractorResourceRef").as[Int] mustBe 42
    }

    "must omit subcontractorResourceRef when absent" in {
      val model = AmendSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "soletrader",
        updatedDetails = baseIndividualDetails
      )
      (Json.toJson(model) \ "subcontractorResourceRef").toOption mustBe None
    }

    "must have auditType AmendSubcontractor" in {
      AmendSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "soletrader",
        updatedDetails = baseIndividualDetails
      ).auditType mustBe "AmendSubcontractor"
    }
  }

  "AmendCompanySubcontractorAuditEventModel" - {

    "must serialise updatedDetails in full" in {
      val model = AmendCompanySubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "company",
        updatedDetails = baseCompanyDetails
      )
      val json  = Json.toJson(model)
      (json \ "updatedDetails").as[JsObject] mustEqual Json.toJson(baseCompanyDetails).as[JsObject]
      (json \ "originalDetails").toOption mustBe None
    }

    "must include subcontractorResourceRef in JSON when present" in {
      val model = AmendCompanySubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = Some(7L),
        typeOfSubcontractor = "company",
        updatedDetails = baseCompanyDetails
      )
      (Json.toJson(model) \ "subcontractorResourceRef").as[Int] mustBe 7
    }

    "must have auditType AmendSubcontractor" in {
      AmendCompanySubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "company",
        updatedDetails = baseCompanyDetails
      ).auditType mustBe "AmendSubcontractor"
    }
  }

  "AmendPartnershipSubcontractorAuditEventModel" - {

    "must serialise updatedDetails in full" in {
      val model = AmendPartnershipSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "partnership",
        updatedDetails = basePartnershipDetails
      )
      val json  = Json.toJson(model)
      (json \ "updatedDetails").as[JsObject] mustEqual Json.toJson(basePartnershipDetails).as[JsObject]
      (json \ "originalDetails").toOption mustBe None
    }

    "must include subcontractorResourceRef in JSON when present" in {
      val model = AmendPartnershipSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = Some(3L),
        typeOfSubcontractor = "partnership",
        updatedDetails = basePartnershipDetails
      )
      (Json.toJson(model) \ "subcontractorResourceRef").as[Int] mustBe 3
    }

    "must have auditType AmendSubcontractor" in {
      AmendPartnershipSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "partnership",
        updatedDetails = basePartnershipDetails
      ).auditType mustBe "AmendSubcontractor"
    }
  }

  "AmendTrustSubcontractorAuditEventModel" - {

    "must serialise updatedDetails in full" in {
      val model = AmendTrustSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "trust",
        updatedDetails = baseTrustDetails
      )
      val json  = Json.toJson(model)
      (json \ "updatedDetails").as[JsObject] mustEqual Json.toJson(baseTrustDetails).as[JsObject]
      (json \ "originalDetails").toOption mustBe None
    }

    "must include subcontractorResourceRef in JSON when present" in {
      val model = AmendTrustSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = Some(99L),
        typeOfSubcontractor = "trust",
        updatedDetails = baseTrustDetails
      )
      (Json.toJson(model) \ "subcontractorResourceRef").as[Int] mustBe 99
    }

    "must have auditType AmendSubcontractor" in {
      AmendTrustSubcontractorAuditEventModel(
        cisId = None,
        subbieResourceRef = None,
        typeOfSubcontractor = "trust",
        updatedDetails = baseTrustDetails
      ).auditType mustBe "AmendSubcontractor"
    }
  }

  "AddTrustSubcontractorAuditEventModel" - {

    "must serialise with only the mandatory field when all optionals are absent" in {
      val model = AddTrustSubcontractorAuditEventModel(
        cisId = None,
        typeOfSubcontractor = "trust",
        trustName = None,
        trustAddressYesNo = None,
        trustAddress = None,
        addTrustContactMethodsYesNo = None,
        trustEmailContactMethod = None,
        trustPhoneContactMethod = None,
        trustMobileContactMethod = None,
        trustEmailAddress = None,
        trustPhoneNumber = None,
        trustMobileNumber = None,
        trustUtrYesNo = None,
        trustUtr = None,
        trustWorksReferenceYesNo = None,
        trustWorksReference = None
      )
      Json.toJson(model) mustEqual Json.obj("typeOfSubcontractor" -> "trust")
    }

    "must serialise all fields when all are present" in {
      val model = AddTrustSubcontractorAuditEventModel(
        cisId = Some("1"),
        typeOfSubcontractor = "trust",
        trustName = Some("Test Trust"),
        trustAddressYesNo = Some(true),
        trustAddress = Some(address),
        addTrustContactMethodsYesNo = Some(true),
        trustEmailContactMethod = Some(true),
        trustPhoneContactMethod = Some(false),
        trustMobileContactMethod = Some(false),
        trustEmailAddress = Some("trust@test.com"),
        trustPhoneNumber = Some("01912170507"),
        trustMobileNumber = Some("+447960141611"),
        trustUtrYesNo = Some(true),
        trustUtr = Some("1111122222"),
        trustWorksReferenceYesNo = Some(true),
        trustWorksReference = Some("WORKREF-004")
      )
      Json.toJson(model) mustEqual Json.obj(
        "cisId"                       -> "1",
        "typeOfSubcontractor"         -> "trust",
        "trustName"                   -> "Test Trust",
        "trustAddressYesNo"           -> true,
        "trustAddress"                -> Json.toJson(address)(Address.auditWrites),
        "addTrustContactMethodsYesNo" -> true,
        "trustEmailContactMethod"     -> true,
        "trustPhoneContactMethod"     -> false,
        "trustMobileContactMethod"    -> false,
        "trustEmailAddress"           -> "trust@test.com",
        "trustPhoneNumber"            -> "01912170507",
        "trustMobileNumber"           -> "+447960141611",
        "trustUtrYesNo"               -> true,
        "trustUtr"                    -> "1111122222",
        "trustWorksReferenceYesNo"    -> true,
        "trustWorksReference"         -> "WORKREF-004"
      )
    }
  }
}
