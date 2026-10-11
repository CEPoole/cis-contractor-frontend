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

package controllers.add

import controllers.actions.*
import forms.mappings.Constants.MaxLength35
import models.{Mode, UserAnswers}
import models.address.{Address, AddressLookupJourneyIdentifier, MandatoryFieldsConfigModel}
import models.requests.DataRequest
import play.api.Logging
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, Call, Result}
import queries.Settable
import repositories.SessionRepository
import services.AddressLookupService
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import queries.AddressLookupAmendReturnQuery

import scala.concurrent.{ExecutionContext, Future}

/** Shared behaviour for the Address Lookup Frontend (ALF) journeys. Each subcontractor type (individual, company,
  * partnership, trust) supplies only the values that differ between journeys; the redirect-to-ALF and callback handling
  * is implemented once here.
  */
trait AddressLookupJourneyController extends FrontendBaseController with I18nSupport with Logging {

  protected val sessionRepository: SessionRepository
  protected val addressLookupService: AddressLookupService
  protected val identify: IdentifierAction
  protected val getData: DataRetrievalAction
  protected val requireData: DataRequiredAction
  protected implicit val executionContext: ExecutionContext

  /** The ALF journey identifier for this subcontractor type. */
  protected def journeyId: AddressLookupJourneyIdentifier.Value

  /** The page the confirmed address is persisted to. */
  protected def addressPage: Settable[Address]

  /** The subcontractor/company/partnership/trust name shown in the ALF page headings. */
  protected def subcontractorName(userAnswers: UserAnswers, mode: Mode)(implicit messages: Messages): Option[String]

  /** Callback ALF returns to for the standard (non-change) flow. */
  protected def standardCallback(mode: Mode): Call

  /** Callback ALF returns to when amending an existing answer. */
  protected def changeCallback(mode: Mode): Call

  /** Where to go after the address is saved in the standard flow. */
  protected def onCompletion(mode: Mode): Call

  /** Where to go after the address is saved in the change flow.
    * @param isAmend
    *   true when the change originates from the amend journey, false for the add/check journey.
    */
  protected def onChangeCompletion(isAmend: Boolean): Call

  private val mandatoryFields = MandatoryFieldsConfigModel(
    addressLine1 = Some(true),
    town = Some(true),
    postcode = Some(true)
  )

  private def journeyRecovery: Call = controllers.routes.JourneyRecoveryController.onPageLoad()

  def redirectToAddressLookup(mode: Mode, changeRoute: Option[String] = None): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      val callback = if (changeRoute.isDefined) changeCallback(mode) else standardCallback(mode)
      subcontractorName(request.userAnswers, mode) match {
        case Some(name) =>
          addressLookupService
            .getJourneyUrl(
              journeyId,
              callback,
              optName = Some(name),
              mandatoryFieldsConfigModel = mandatoryFields,
              line1MaxLength = Some(MaxLength35),
              line2MaxLength = Some(MaxLength35),
              line3MaxLength = Some(MaxLength35),
              townMaxLength = Some(MaxLength35)
            )
            .map(Redirect)
            .recover { case ex =>
              logger.error(
                "[AddressLookupJourneyController][redirectToAddressLookup] - failed to get ALF journey URL",
                ex
              )
              Redirect(journeyRecovery)
            }
        case None       =>
          logger.error(
            "[AddressLookupJourneyController][redirectToAddressLookup] - subcontractor name missing from userAnswers"
          )
          Future.successful(Redirect(journeyRecovery))
      }
    }

  def addressLookupCallback(id: String, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      saveAddressAndRedirect(id, onCompletion(mode))
    }

  def addressLookupCallbackChange(id: String, mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      val isAmend = request.userAnswers.get(AddressLookupAmendReturnQuery).getOrElse(false)
      saveAddressAndRedirect(id, onChangeCompletion(isAmend))
    }

  private def saveAddressAndRedirect(id: String, onSuccess: Call)(implicit
    request: DataRequest[AnyContent]
  ): Future[Result] =
    (for {
      address <- addressLookupService.getAddressById(id)
      updated <- addressLookupService.saveAddressDetails(address, addressPage)
    } yield
      if (updated) Redirect(onSuccess)
      else {
        logger.error(
          "[AddressLookupJourneyController][saveAddressAndRedirect] - address not saved, update returned false"
        )
        Redirect(journeyRecovery)
      })
      .recover { case ex =>
        logger.error(
          "[AddressLookupJourneyController][saveAddressAndRedirect] - failed to retrieve or save address",
          ex
        )
        Redirect(journeyRecovery)
      }

}
