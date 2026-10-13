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

import play.api.libs.json.*
import play.twirl.api.{Html, HtmlFormat}

case class Country(code: Option[String], name: Option[String])

object Country {
  implicit val format: OFormat[Country] = Json.format[Country]
}

case class Address(
  addressLine1: String,
  addressLine2: Option[String] = None,
  addressLine3: Option[String] = None,
  addressLine4: Option[String] = None,
  addressLine5: Option[String] = None,
  postcode: Option[String] = None,
  country: Option[Country] = None,
  addressValidated: Boolean = false,
  uprn: Option[String] = None
) {

  val id: String =
    Seq(Some(addressLine1), if (postcode.isDefined) postcode else country).flatten.mkString.replaceAll(" ", "")

  /** * trims spaces and converts any Some(" ") to None
    */
  def normalise(): Address =
    Address(
      this.addressLine1.trim,
      this.addressLine2 map (_.trim) filterNot (_.isEmpty),
      this.addressLine3 map (_.trim) filterNot (_.isEmpty),
      this.addressLine4 map (_.trim) filterNot (_.isEmpty),
      this.addressLine5 map (_.trim) filterNot (_.isEmpty),
      this.postcode map (_.trim) filterNot (_.isEmpty),
      this.country.flatMap { country =>
        val optCode = country.code.map(_.trim).filterNot(_.isEmpty)
        val optName = country.name.map(_.trim).filterNot(_.isEmpty)

        (optCode, optName) match {
          case (None, None) =>
            None
          case _            =>
            Some(Country(code = optCode, name = optName))
        }
      },
      this.addressValidated,
      this.uprn
    )
}

object Address {

  val auditWrites: OWrites[Address] = OWrites { address =>
    Json.obj("addressLine1" -> address.addressLine1) ++
      address.addressLine2.fold(Json.obj())(v => Json.obj("addressLine2" -> v)) ++
      address.addressLine3.fold(Json.obj())(v => Json.obj("addressLine3" -> v)) ++
      address.addressLine4.fold(Json.obj())(v => Json.obj("addressLine4" -> v)) ++
      address.addressLine5.fold(Json.obj())(v => Json.obj("addressLine5" -> v)) ++
      address.postcode.fold(Json.obj())(v => Json.obj("postcode" -> v)) ++
      address.country.flatMap(_.code).fold(Json.obj())(v => Json.obj("countryCode" -> v)) ++
      address.country.flatMap(_.name).fold(Json.obj())(v => Json.obj("countryName" -> v)) ++
      address.uprn.fold(Json.obj())(v => Json.obj("uprn" -> v)) ++
      Json.obj("addressValidated" -> address.addressValidated)
  }

  private sealed trait AddressLineOrPostcode

  private final case class AddressLine(line: String) extends AddressLineOrPostcode

  private final case class Postcode(postcode: String) extends AddressLineOrPostcode

  implicit val format: OFormat[Address] = Json.format[Address]

  object addressLookupReads extends Reads[Address] {
    def reads(json: JsValue): JsResult[Address] = {
      val address          = (json \ "address").as[JsObject]
      val postcode         = (address \ "postcode").asOpt[String]
      val lineMap          = (address \ "lines").as[List[String]].zipWithIndex.map(_.swap).toMap
      val country          = (address \ "country").asOpt[Country]
      val addressValidated = (json \ "id").isDefined
      val uprn             = (json \ "id").asOpt[String]

      if (postcode.isEmpty && country.isEmpty) {
        JsError(JsonValidationError("Neither postcode nor country were defined"))
      } else if (lineMap.isEmpty) {
        JsError(JsonValidationError(s"ALF returned no address lines"))
      } else {
        JsSuccess(
          Address(
            lineMap(0),
            lineMap.get(1),
            lineMap.get(2),
            lineMap.get(3),
            lineMap.get(4),
            postcode,
            country,
            addressValidated,
            uprn
          )
        )
      }
    }
  }

  def normalisedSeq(address: Address): Seq[String] =
    Seq[Option[AddressLineOrPostcode]](
      Option(AddressLine(address.addressLine1)),
      address.addressLine2.map(AddressLine(_)),
      address.addressLine3.map(AddressLine(_)),
      address.addressLine4.map(AddressLine(_)),
      address.addressLine5.map(AddressLine(_)),
      address.postcode.map(Postcode(_)),
      address.country.flatMap(_.name).map(AddressLine(_))
    ).collect {
      case Some(AddressLine(line))  => line.split("\\s").map(_.capitalize).mkString(" ")
      case Some(Postcode(postcode)) => postcode.toUpperCase()
    }

  def toHtml(address: Address): Html = Html(
    normalisedSeq(address).map(line => HtmlFormat.escape(line)).mkString("<br>")
  )
}
