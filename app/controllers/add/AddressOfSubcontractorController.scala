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

package controllers.add

import controllers.actions.*
import models.address.Address
import models.address.AddressLookupJourneyIdentifier.individualQuestionsAddress
import models.{AmendMode, FinalValidationMode, Mode, UserAnswers}
import pages.add.AddressOfSubcontractorPage
import play.api.i18n.{Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents}
import queries.{AddressLookupAmendReturnQuery, Settable}
import repositories.SessionRepository
import services.AddressLookupService
import utils.SubcontractorNameExtractor

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AddressOfSubcontractorController @Inject() (
  override val messagesApi: MessagesApi,
  override protected val sessionRepository: SessionRepository,
  override protected val identify: IdentifierAction,
  override protected val getData: DataRetrievalAction,
  override protected val requireData: DataRequiredAction,
  override protected val addressLookupService: AddressLookupService,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  val controllerComponents: MessagesControllerComponents
)(implicit override protected val executionContext: ExecutionContext)
    extends AddressLookupJourneyController {

  override protected def journeyId = individualQuestionsAddress

  override protected def addressPage: Settable[Address] = AddressOfSubcontractorPage

  override protected def subcontractorName(userAnswers: UserAnswers, mode: Mode)(implicit
    messages: Messages
  ): Option[String] =
    subcontractorNameExtractor.getSubcontractorName(userAnswers, mode)

  override protected def standardCallback(mode: Mode): Call =
    routes.AddressOfSubcontractorController.addressLookupCallback(id = "", mode = mode)

  override protected def changeCallback(mode: Mode): Call =
    routes.AddressOfSubcontractorController.addressLookupCallbackChange(id = "", mode = mode)

  override protected def onCompletion(mode: Mode): Call =
    mode match {
      case FinalValidationMode =>
        controllers.finalvalidations.routes.FinalValidationCompleteController.onPageLoad()
      case _                   =>
        routes.AddIndividualContactMethodsYesNoController.onPageLoad(mode)
    }

  override protected def onChangeCompletion(isAmend: Boolean): Call =
    if (isAmend) controllers.amend.routes.AmendIndividualCheckYourAnswersController.onPageLoad()
    else routes.CheckYourAnswersController.onPageLoad()

  def redirectToAmendAddressLookup(): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      (for {
        ua <- Future.fromTry(request.userAnswers.set(AddressLookupAmendReturnQuery, true))
        _  <- sessionRepository.set(ua)
      } yield Redirect(routes.AddressOfSubcontractorController.redirectToAddressLookup(AmendMode, Some("change"))))
        .recover { case ex =>
          logger.error(
            "[AddressOfSubcontractorController][redirectToAmendAddressLookup] - failed to set amend flag",
            ex
          )
          Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
        }
    }
}
