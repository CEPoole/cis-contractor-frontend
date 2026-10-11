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

import models.address.Address
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.json.{Format, JsValue, Json, OWrites, __}
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

trait AuditEvent {
  val auditType: String
}

trait AuditEventModel extends AuditEvent {
  private val auditSource: String = "cis-contractor-frontend"

  val detailJson: JsValue

  def extendedDataEvent: ExtendedDataEvent =
    ExtendedDataEvent(
      auditSource = auditSource,
      auditType = auditType,
      detail = detailJson
    )
}

case class AuthFailureAuditEventModel(
  agentUserId: String,
  taxOfficeNumber: String,
  taxOfficeReference: String,
  clientUniqueId: String
) extends AuditEventModel {
  override val auditType: String = "AuthoriseServiceGuardFailure"

  override val detailJson: JsValue =
    Json.toJson(this)(AuthFailureAuditEventModel.formats)
}

object AuthFailureAuditEventModel {
  implicit val formats: Format[AuthFailureAuditEventModel] =
    Json.format[AuthFailureAuditEventModel]
}

case class AddSubcontractorAuditEventModel(
  cisId: Option[String],
  typeOfSubcontractor: String,
  subcontractorNameSelected: Option[Boolean],
  tradingNameSelected: Option[Boolean],
  firstName: Option[String],
  middleName: Option[String],
  surname: Option[String],
  tradingNameOfSubcontractor: Option[String],
  subAddressYesNo: Option[Boolean],
  addressOfSubcontractor: Option[Address],
  addIndividualContactMethodsYesNo: Option[Boolean],
  individualEmailContactMethod: Option[Boolean],
  individualPhoneContactMethod: Option[Boolean],
  individualMobileContactMethod: Option[Boolean],
  individualEmailAddress: Option[String],
  individualPhoneNumber: Option[String],
  individualMobileNumber: Option[String],
  uniqueTaxpayerReferenceYesNo: Option[Boolean],
  subcontractorsUniqueTaxpayerReference: Option[String],
  nationalInsuranceNumberYesNo: Option[Boolean],
  subNationalInsuranceNumber: Option[String],
  worksReferenceNumberYesNo: Option[Boolean],
  worksReferenceNumber: Option[String]
) extends AuditEvent {
  override val auditType: String = "AddSubcontractor"
}

object AddSubcontractorAuditEventModel {
  implicit val writes: OWrites[AddSubcontractorAuditEventModel] = OWrites { model =>
    model.cisId.fold(Json.obj())(v => Json.obj("cisId" -> v)) ++
      Json.obj("typeOfSubcontractor" -> model.typeOfSubcontractor) ++
      model.subcontractorNameSelected.fold(Json.obj())(v => Json.obj("subcontractorNameSelected" -> v)) ++
      model.tradingNameSelected.fold(Json.obj())(v => Json.obj("tradingNameSelected" -> v)) ++
      model.firstName.fold(Json.obj())(v => Json.obj("firstName" -> v)) ++
      model.middleName.fold(Json.obj())(v => Json.obj("middleName" -> v)) ++
      model.surname.fold(Json.obj())(v => Json.obj("surname" -> v)) ++
      model.tradingNameOfSubcontractor.fold(Json.obj())(v => Json.obj("tradingNameOfSubcontractor" -> v)) ++
      model.subAddressYesNo.fold(Json.obj())(v => Json.obj("subcontractorAddressYesNo" -> v)) ++
      model.addressOfSubcontractor.fold(Json.obj())(v =>
        Json.obj("addressOfSubcontractor" -> Json.toJson(v)(Address.auditWrites))
      ) ++
      model.addIndividualContactMethodsYesNo.fold(Json.obj())(v => Json.obj("addIndividualContactMethodsYesNo" -> v)) ++
      model.individualEmailContactMethod.fold(Json.obj())(v => Json.obj("individualEmailContactMethod" -> v)) ++
      model.individualPhoneContactMethod.fold(Json.obj())(v => Json.obj("individualPhoneContactMethod" -> v)) ++
      model.individualMobileContactMethod.fold(Json.obj())(v => Json.obj("individualMobileContactMethod" -> v)) ++
      model.individualEmailAddress.fold(Json.obj())(v => Json.obj("individualEmailAddress" -> v)) ++
      model.individualPhoneNumber.fold(Json.obj())(v => Json.obj("individualPhoneNumber" -> v)) ++
      model.individualMobileNumber.fold(Json.obj())(v => Json.obj("individualMobileNumber" -> v)) ++
      model.uniqueTaxpayerReferenceYesNo.fold(Json.obj())(v => Json.obj("uniqueTaxpayerReferenceYesNo" -> v)) ++
      model.subcontractorsUniqueTaxpayerReference.fold(Json.obj())(v =>
        Json.obj("subcontractorsUniqueTaxpayerReference" -> v)
      ) ++
      model.nationalInsuranceNumberYesNo.fold(Json.obj())(v => Json.obj("nationalInsuranceNumberYesNo" -> v)) ++
      model.subNationalInsuranceNumber.fold(Json.obj())(v => Json.obj("subcontractorNationalInsuranceNumber" -> v)) ++
      model.worksReferenceNumberYesNo.fold(Json.obj())(v => Json.obj("worksReferenceNumberYesNo" -> v)) ++
      model.worksReferenceNumber.fold(Json.obj())(v => Json.obj("worksReferenceNumber" -> v))
  }
}

case class AddCompanySubcontractorAuditEventModel(
  cisId: Option[String],
  typeOfSubcontractor: String,
  companyName: Option[String],
  companyAddressYesNo: Option[Boolean],
  companyAddress: Option[Address],
  addCompanyContactMethodsYesNo: Option[Boolean],
  companyEmailContactMethod: Option[Boolean],
  companyPhoneContactMethod: Option[Boolean],
  companyMobileContactMethod: Option[Boolean],
  companyEmailAddress: Option[String],
  companyPhoneNumber: Option[String],
  companyMobileNumber: Option[String],
  companyUtrYesNo: Option[Boolean],
  companyUtr: Option[String],
  companyCrnYesNo: Option[Boolean],
  companyCrn: Option[String],
  companyWorksReferenceYesNo: Option[Boolean],
  companyWorksReference: Option[String]
) extends AuditEvent {
  override val auditType: String = "AddSubcontractor"
}

object AddCompanySubcontractorAuditEventModel {
  implicit val writes: OWrites[AddCompanySubcontractorAuditEventModel] = (
    (__ \ "cisId").writeNullable[String] and
      (__ \ "typeOfSubcontractor").write[String] and
      (__ \ "companyName").writeNullable[String] and
      (__ \ "companyAddressYesNo").writeNullable[Boolean] and
      (__ \ "companyAddress").writeNullable(Address.auditWrites) and
      (__ \ "addCompanyContactMethodsYesNo").writeNullable[Boolean] and
      (__ \ "companyEmailContactMethod").writeNullable[Boolean] and
      (__ \ "companyPhoneContactMethod").writeNullable[Boolean] and
      (__ \ "companyMobileContactMethod").writeNullable[Boolean] and
      (__ \ "companyEmailAddress").writeNullable[String] and
      (__ \ "companyPhoneNumber").writeNullable[String] and
      (__ \ "companyMobileNumber").writeNullable[String] and
      (__ \ "companyUtrYesNo").writeNullable[Boolean] and
      (__ \ "companyUtr").writeNullable[String] and
      (__ \ "companyCrnYesNo").writeNullable[Boolean] and
      (__ \ "companyCrn").writeNullable[String] and
      (__ \ "companyWorksReferenceYesNo").writeNullable[Boolean] and
      (__ \ "companyWorksReference").writeNullable[String]
  )(Tuple.fromProductTyped(_))
}

case class AddPartnershipSubcontractorAuditEventModel(
  cisId: Option[String],
  typeOfSubcontractor: String,
  partnershipName: Option[String],
  partnershipAddressYesNo: Option[Boolean],
  partnershipAddress: Option[Address],
  addPartnershipContactMethodsYesNo: Option[Boolean],
  partnershipEmailContactMethod: Option[Boolean],
  partnershipPhoneContactMethod: Option[Boolean],
  partnershipMobileContactMethod: Option[Boolean],
  partnershipEmailAddress: Option[String],
  partnershipPhoneNumber: Option[String],
  partnershipMobileNumber: Option[String],
  partnershipHasUtrYesNo: Option[Boolean],
  partnershipUniqueTaxpayerReference: Option[String],
  partnershipNominatedPartnerName: Option[String],
  partnershipNominatedPartnerUtrYesNo: Option[Boolean],
  partnershipNominatedPartnerUtr: Option[String],
  partnershipNominatedPartnerNinoYesNo: Option[Boolean],
  nominatedPartnerNationalInsuranceNumber: Option[String],
  partnershipNominatedPartnerCrnYesNo: Option[Boolean],
  nominatedPartnerCompanyRegistrationNumber: Option[String],
  partnershipWorksReferenceNumberYesNo: Option[Boolean],
  partnershipWorksReference: Option[String]
) extends AuditEvent {
  override val auditType: String = "AddSubcontractor"
}

object AddPartnershipSubcontractorAuditEventModel {
  implicit val writes: OWrites[AddPartnershipSubcontractorAuditEventModel] = OWrites { model =>
    model.cisId.fold(Json.obj())(v => Json.obj("cisId" -> v)) ++
      Json.obj("typeOfSubcontractor" -> model.typeOfSubcontractor) ++
      model.partnershipName.fold(Json.obj())(v => Json.obj("partnershipName" -> v)) ++
      model.partnershipAddressYesNo.fold(Json.obj())(v => Json.obj("partnershipAddressYesNo" -> v)) ++
      model.partnershipAddress.fold(Json.obj())(v =>
        Json.obj("partnershipAddress" -> Json.toJson(v)(Address.auditWrites))
      ) ++
      model.addPartnershipContactMethodsYesNo.fold(Json.obj())(v =>
        Json.obj("addPartnershipContactMethodsYesNo" -> v)
      ) ++
      model.partnershipEmailContactMethod.fold(Json.obj())(v => Json.obj("partnershipEmailContactMethod" -> v)) ++
      model.partnershipPhoneContactMethod.fold(Json.obj())(v => Json.obj("partnershipPhoneContactMethod" -> v)) ++
      model.partnershipMobileContactMethod.fold(Json.obj())(v => Json.obj("partnershipMobileContactMethod" -> v)) ++
      model.partnershipEmailAddress.fold(Json.obj())(v => Json.obj("partnershipEmailAddress" -> v)) ++
      model.partnershipPhoneNumber.fold(Json.obj())(v => Json.obj("partnershipPhoneNumber" -> v)) ++
      model.partnershipMobileNumber.fold(Json.obj())(v => Json.obj("partnershipMobileNumber" -> v)) ++
      model.partnershipHasUtrYesNo.fold(Json.obj())(v => Json.obj("partnershipHasUtrYesNo" -> v)) ++
      model.partnershipUniqueTaxpayerReference.fold(Json.obj())(v =>
        Json.obj("partnershipUniqueTaxpayerReference" -> v)
      ) ++
      model.partnershipNominatedPartnerName.fold(Json.obj())(v => Json.obj("partnershipNominatedPartnerName" -> v)) ++
      model.partnershipNominatedPartnerUtrYesNo.fold(Json.obj())(v =>
        Json.obj("partnershipNominatedPartnerUtrYesNo" -> v)
      ) ++
      model.partnershipNominatedPartnerUtr.fold(Json.obj())(v => Json.obj("partnershipNominatedPartnerUtr" -> v)) ++
      model.partnershipNominatedPartnerNinoYesNo.fold(Json.obj())(v =>
        Json.obj("partnershipNominatedPartnerNinoYesNo" -> v)
      ) ++
      model.nominatedPartnerNationalInsuranceNumber.fold(Json.obj())(v =>
        Json.obj("nominatedPartnerNationalInsuranceNumber" -> v)
      ) ++
      model.partnershipNominatedPartnerCrnYesNo.fold(Json.obj())(v =>
        Json.obj("partnershipNominatedPartnerCrnYesNo" -> v)
      ) ++
      model.nominatedPartnerCompanyRegistrationNumber.fold(Json.obj())(v =>
        Json.obj("nominatedPartnerCompanyRegistrationNumber" -> v)
      ) ++
      model.partnershipWorksReferenceNumberYesNo.fold(Json.obj())(v =>
        Json.obj("partnershipWorksReferenceNumberYesNo" -> v)
      ) ++
      model.partnershipWorksReference.fold(Json.obj())(v => Json.obj("partnershipWorksReference" -> v))
  }
}

case class AddTrustSubcontractorAuditEventModel(
  cisId: Option[String],
  typeOfSubcontractor: String,
  trustName: Option[String],
  trustAddressYesNo: Option[Boolean],
  trustAddress: Option[Address],
  addTrustContactMethodsYesNo: Option[Boolean],
  trustEmailContactMethod: Option[Boolean],
  trustPhoneContactMethod: Option[Boolean],
  trustMobileContactMethod: Option[Boolean],
  trustEmailAddress: Option[String],
  trustPhoneNumber: Option[String],
  trustMobileNumber: Option[String],
  trustUtrYesNo: Option[Boolean],
  trustUtr: Option[String],
  trustWorksReferenceYesNo: Option[Boolean],
  trustWorksReference: Option[String]
) extends AuditEvent {
  override val auditType: String = "AddSubcontractor"
}

object AddTrustSubcontractorAuditEventModel {
  implicit val writes: OWrites[AddTrustSubcontractorAuditEventModel] = (
    (__ \ "cisId").writeNullable[String] and
      (__ \ "typeOfSubcontractor").write[String] and
      (__ \ "trustName").writeNullable[String] and
      (__ \ "trustAddressYesNo").writeNullable[Boolean] and
      (__ \ "trustAddress").writeNullable(Address.auditWrites) and
      (__ \ "addTrustContactMethodsYesNo").writeNullable[Boolean] and
      (__ \ "trustEmailContactMethod").writeNullable[Boolean] and
      (__ \ "trustPhoneContactMethod").writeNullable[Boolean] and
      (__ \ "trustMobileContactMethod").writeNullable[Boolean] and
      (__ \ "trustEmailAddress").writeNullable[String] and
      (__ \ "trustPhoneNumber").writeNullable[String] and
      (__ \ "trustMobileNumber").writeNullable[String] and
      (__ \ "trustUtrYesNo").writeNullable[Boolean] and
      (__ \ "trustUtr").writeNullable[String] and
      (__ \ "trustWorksReferenceYesNo").writeNullable[Boolean] and
      (__ \ "trustWorksReference").writeNullable[String]
  )(Tuple.fromProductTyped(_))
}

case class IndividualSubcontractorDetails(
  subcontractorNameSelected: Option[Boolean],
  tradingNameSelected: Option[Boolean],
  firstName: Option[String],
  middleName: Option[String],
  surname: Option[String],
  tradingNameOfSubcontractor: Option[String],
  subAddressYesNo: Option[Boolean],
  addressOfSubcontractor: Option[Address],
  addIndividualContactMethodsYesNo: Option[Boolean],
  individualEmailContactMethod: Option[Boolean],
  individualPhoneContactMethod: Option[Boolean],
  individualMobileContactMethod: Option[Boolean],
  individualEmailAddress: Option[String],
  individualPhoneNumber: Option[String],
  individualMobileNumber: Option[String],
  uniqueTaxpayerReferenceYesNo: Option[Boolean],
  subcontractorsUniqueTaxpayerReference: Option[String],
  nationalInsuranceNumberYesNo: Option[Boolean],
  subNationalInsuranceNumber: Option[String],
  worksReferenceNumberYesNo: Option[Boolean],
  worksReferenceNumber: Option[String]
)

object IndividualSubcontractorDetails {
  implicit val writes: OWrites[IndividualSubcontractorDetails] = (
    (__ \ "subcontractorNameSelected").writeNullable[Boolean] and
      (__ \ "tradingNameSelected").writeNullable[Boolean] and
      (__ \ "firstName").writeNullable[String] and
      (__ \ "middleName").writeNullable[String] and
      (__ \ "surname").writeNullable[String] and
      (__ \ "tradingNameOfSubcontractor").writeNullable[String] and
      (__ \ "subcontractorAddressYesNo").writeNullable[Boolean] and
      (__ \ "addressOfSubcontractor").writeNullable(Address.auditWrites) and
      (__ \ "addIndividualContactMethodsYesNo").writeNullable[Boolean] and
      (__ \ "individualEmailContactMethod").writeNullable[Boolean] and
      (__ \ "individualPhoneContactMethod").writeNullable[Boolean] and
      (__ \ "individualMobileContactMethod").writeNullable[Boolean] and
      (__ \ "individualEmailAddress").writeNullable[String] and
      (__ \ "individualPhoneNumber").writeNullable[String] and
      (__ \ "individualMobileNumber").writeNullable[String] and
      (__ \ "uniqueTaxpayerReferenceYesNo").writeNullable[Boolean] and
      (__ \ "subcontractorsUniqueTaxpayerReference").writeNullable[String] and
      (__ \ "nationalInsuranceNumberYesNo").writeNullable[Boolean] and
      (__ \ "subcontractorNationalInsuranceNumber").writeNullable[String] and
      (__ \ "worksReferenceNumberYesNo").writeNullable[Boolean] and
      (__ \ "worksReferenceNumber").writeNullable[String]
  )(Tuple.fromProductTyped(_))
}

case class AmendSubcontractorAuditEventModel(
  cisId: Option[String],
  subbieResourceRef: Option[Long],
  typeOfSubcontractor: String,
  updatedDetails: IndividualSubcontractorDetails
) extends AuditEvent {
  override val auditType: String = "AmendSubcontractor"
}

object AmendSubcontractorAuditEventModel {
  implicit val writes: OWrites[AmendSubcontractorAuditEventModel] = OWrites { model =>
    val base      = Json.obj("typeOfSubcontractor" -> model.typeOfSubcontractor) ++
      Json.obj("updatedDetails" -> Json.toJson(model.updatedDetails))
    val withCisId = model.cisId.fold(base)(id => Json.obj("cisId" -> id) ++ base)
    model.subbieResourceRef.fold(withCisId)(ref => withCisId ++ Json.obj("subcontractorResourceRef" -> ref))
  }
}

case class CompanySubcontractorDetails(
  companyName: Option[String],
  companyAddressYesNo: Option[Boolean],
  companyAddress: Option[Address],
  addCompanyContactMethodsYesNo: Option[Boolean],
  companyEmailContactMethod: Option[Boolean],
  companyPhoneContactMethod: Option[Boolean],
  companyMobileContactMethod: Option[Boolean],
  companyEmailAddress: Option[String],
  companyPhoneNumber: Option[String],
  companyMobileNumber: Option[String],
  companyUtrYesNo: Option[Boolean],
  companyUtr: Option[String],
  companyCrnYesNo: Option[Boolean],
  companyCrn: Option[String],
  companyWorksReferenceYesNo: Option[Boolean],
  companyWorksReference: Option[String]
)

object CompanySubcontractorDetails {
  implicit val writes: OWrites[CompanySubcontractorDetails] = (
    (__ \ "companyName").writeNullable[String] and
      (__ \ "companyAddressYesNo").writeNullable[Boolean] and
      (__ \ "companyAddress").writeNullable(Address.auditWrites) and
      (__ \ "addCompanyContactMethodsYesNo").writeNullable[Boolean] and
      (__ \ "companyEmailContactMethod").writeNullable[Boolean] and
      (__ \ "companyPhoneContactMethod").writeNullable[Boolean] and
      (__ \ "companyMobileContactMethod").writeNullable[Boolean] and
      (__ \ "companyEmailAddress").writeNullable[String] and
      (__ \ "companyPhoneNumber").writeNullable[String] and
      (__ \ "companyMobileNumber").writeNullable[String] and
      (__ \ "companyUtrYesNo").writeNullable[Boolean] and
      (__ \ "companyUtr").writeNullable[String] and
      (__ \ "companyCrnYesNo").writeNullable[Boolean] and
      (__ \ "companyCrn").writeNullable[String] and
      (__ \ "companyWorksReferenceYesNo").writeNullable[Boolean] and
      (__ \ "companyWorksReference").writeNullable[String]
  )(Tuple.fromProductTyped(_))
}

case class AmendCompanySubcontractorAuditEventModel(
  cisId: Option[String],
  subbieResourceRef: Option[Long],
  typeOfSubcontractor: String,
  updatedDetails: CompanySubcontractorDetails
) extends AuditEvent {
  override val auditType: String = "AmendSubcontractor"
}

object AmendCompanySubcontractorAuditEventModel {
  implicit val writes: OWrites[AmendCompanySubcontractorAuditEventModel] = OWrites { model =>
    val base      = Json.obj("typeOfSubcontractor" -> model.typeOfSubcontractor) ++
      Json.obj("updatedDetails" -> Json.toJson(model.updatedDetails))
    val withCisId = model.cisId.fold(base)(id => Json.obj("cisId" -> id) ++ base)
    model.subbieResourceRef.fold(withCisId)(ref => withCisId ++ Json.obj("subcontractorResourceRef" -> ref))
  }
}

case class PartnershipSubcontractorDetails(
  partnershipName: Option[String],
  partnershipAddressYesNo: Option[Boolean],
  partnershipAddress: Option[Address],
  addPartnershipContactMethodsYesNo: Option[Boolean],
  partnershipEmailContactMethod: Option[Boolean],
  partnershipPhoneContactMethod: Option[Boolean],
  partnershipMobileContactMethod: Option[Boolean],
  partnershipEmailAddress: Option[String],
  partnershipPhoneNumber: Option[String],
  partnershipMobileNumber: Option[String],
  partnershipHasUtrYesNo: Option[Boolean],
  partnershipUniqueTaxpayerReference: Option[String],
  partnershipNominatedPartnerName: Option[String],
  partnershipNominatedPartnerUtrYesNo: Option[Boolean],
  partnershipNominatedPartnerUtr: Option[String],
  partnershipNominatedPartnerNinoYesNo: Option[Boolean],
  nominatedPartnerNationalInsuranceNumber: Option[String],
  partnershipNominatedPartnerCrnYesNo: Option[Boolean],
  nominatedPartnerCompanyRegistrationNumber: Option[String],
  partnershipWorksReferenceNumberYesNo: Option[Boolean],
  partnershipWorksReference: Option[String]
)

object PartnershipSubcontractorDetails {
  implicit val writes: OWrites[PartnershipSubcontractorDetails] = (
    (__ \ "partnershipName").writeNullable[String] and
      (__ \ "partnershipAddressYesNo").writeNullable[Boolean] and
      (__ \ "partnershipAddress").writeNullable(Address.auditWrites) and
      (__ \ "addPartnershipContactMethodsYesNo").writeNullable[Boolean] and
      (__ \ "partnershipEmailContactMethod").writeNullable[Boolean] and
      (__ \ "partnershipPhoneContactMethod").writeNullable[Boolean] and
      (__ \ "partnershipMobileContactMethod").writeNullable[Boolean] and
      (__ \ "partnershipEmailAddress").writeNullable[String] and
      (__ \ "partnershipPhoneNumber").writeNullable[String] and
      (__ \ "partnershipMobileNumber").writeNullable[String] and
      (__ \ "partnershipHasUtrYesNo").writeNullable[Boolean] and
      (__ \ "partnershipUniqueTaxpayerReference").writeNullable[String] and
      (__ \ "partnershipNominatedPartnerName").writeNullable[String] and
      (__ \ "partnershipNominatedPartnerUtrYesNo").writeNullable[Boolean] and
      (__ \ "partnershipNominatedPartnerUtr").writeNullable[String] and
      (__ \ "partnershipNominatedPartnerNinoYesNo").writeNullable[Boolean] and
      (__ \ "nominatedPartnerNationalInsuranceNumber").writeNullable[String] and
      (__ \ "partnershipNominatedPartnerCrnYesNo").writeNullable[Boolean] and
      (__ \ "nominatedPartnerCompanyRegistrationNumber").writeNullable[String] and
      (__ \ "partnershipWorksReferenceNumberYesNo").writeNullable[Boolean] and
      (__ \ "partnershipWorksReference").writeNullable[String]
  )(Tuple.fromProductTyped(_))
}

case class AmendPartnershipSubcontractorAuditEventModel(
  cisId: Option[String],
  subbieResourceRef: Option[Long],
  typeOfSubcontractor: String,
  updatedDetails: PartnershipSubcontractorDetails
) extends AuditEvent {
  override val auditType: String = "AmendSubcontractor"
}

object AmendPartnershipSubcontractorAuditEventModel {
  implicit val writes: OWrites[AmendPartnershipSubcontractorAuditEventModel] = OWrites { model =>
    val base      = Json.obj("typeOfSubcontractor" -> model.typeOfSubcontractor) ++
      Json.obj("updatedDetails" -> Json.toJson(model.updatedDetails))
    val withCisId = model.cisId.fold(base)(id => Json.obj("cisId" -> id) ++ base)
    model.subbieResourceRef.fold(withCisId)(ref => withCisId ++ Json.obj("subcontractorResourceRef" -> ref))
  }
}

case class TrustSubcontractorDetails(
  trustName: Option[String],
  trustAddressYesNo: Option[Boolean],
  trustAddress: Option[Address],
  addTrustContactMethodsYesNo: Option[Boolean],
  trustEmailContactMethod: Option[Boolean],
  trustPhoneContactMethod: Option[Boolean],
  trustMobileContactMethod: Option[Boolean],
  trustEmailAddress: Option[String],
  trustPhoneNumber: Option[String],
  trustMobileNumber: Option[String],
  trustUtrYesNo: Option[Boolean],
  trustUtr: Option[String],
  trustWorksReferenceYesNo: Option[Boolean],
  trustWorksReference: Option[String]
)

object TrustSubcontractorDetails {
  implicit val writes: OWrites[TrustSubcontractorDetails] = (
    (__ \ "trustName").writeNullable[String] and
      (__ \ "trustAddressYesNo").writeNullable[Boolean] and
      (__ \ "trustAddress").writeNullable(Address.auditWrites) and
      (__ \ "addTrustContactMethodsYesNo").writeNullable[Boolean] and
      (__ \ "trustEmailContactMethod").writeNullable[Boolean] and
      (__ \ "trustPhoneContactMethod").writeNullable[Boolean] and
      (__ \ "trustMobileContactMethod").writeNullable[Boolean] and
      (__ \ "trustEmailAddress").writeNullable[String] and
      (__ \ "trustPhoneNumber").writeNullable[String] and
      (__ \ "trustMobileNumber").writeNullable[String] and
      (__ \ "trustUtrYesNo").writeNullable[Boolean] and
      (__ \ "trustUtr").writeNullable[String] and
      (__ \ "trustWorksReferenceYesNo").writeNullable[Boolean] and
      (__ \ "trustWorksReference").writeNullable[String]
  )(Tuple.fromProductTyped(_))
}

case class AmendTrustSubcontractorAuditEventModel(
  cisId: Option[String],
  subbieResourceRef: Option[Long],
  typeOfSubcontractor: String,
  updatedDetails: TrustSubcontractorDetails
) extends AuditEvent {
  override val auditType: String = "AmendSubcontractor"
}

object AmendTrustSubcontractorAuditEventModel {
  implicit val writes: OWrites[AmendTrustSubcontractorAuditEventModel] = OWrites { model =>
    val base      = Json.obj("typeOfSubcontractor" -> model.typeOfSubcontractor) ++
      Json.obj("updatedDetails" -> Json.toJson(model.updatedDetails))
    val withCisId = model.cisId.fold(base)(id => Json.obj("cisId" -> id) ++ base)
    model.subbieResourceRef.fold(withCisId)(ref => withCisId ++ Json.obj("subcontractorResourceRef" -> ref))
  }
}
