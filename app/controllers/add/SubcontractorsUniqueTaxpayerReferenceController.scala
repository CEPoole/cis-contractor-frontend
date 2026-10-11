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
import forms.add.UtrFormProvider
import models.{AmendMode, FinalValidationMode, Mode}
import models.requests.DataRequest
import navigation.Navigator
import pages.add.{SubcontractorsUniqueTaxpayerReferencePage, UniqueTaxpayerReferenceYesNoPage}
import pages.finalvalidation.FinalValidationBaseUtrPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.{SubcontractorService, YesOrNoPageGuardService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import utils.SubcontractorNameExtractor
import views.html.add.SubcontractorsUniqueTaxpayerReferenceView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SubcontractorsUniqueTaxpayerReferenceController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: UtrFormProvider,
  subcontractorService: SubcontractorService,
  subcontractorNameExtractor: SubcontractorNameExtractor,
  redirectVerifiedSubcontractor: RedirectVerifiedSubcontractorAction,
  val controllerComponents: MessagesControllerComponents,
  yesOrNoPageGuardService: YesOrNoPageGuardService,
  view: SubcontractorsUniqueTaxpayerReferenceView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private val form = formProvider()

  private def recoveryRedirect =
    Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())

  private def preparedForm(implicit request: DataRequest[?]) =
    request.userAnswers.get(SubcontractorsUniqueTaxpayerReferencePage).fold(form)(form.fill)

  private def saveAndContinue(mode: Mode, value: String)(implicit request: DataRequest[?]) =
    for {
      updatedAnswers <-
        Future.fromTry(request.userAnswers.set(SubcontractorsUniqueTaxpayerReferencePage, value))
      _              <- sessionRepository.set(updatedAnswers)
    } yield Redirect(
      navigator.nextPage(SubcontractorsUniqueTaxpayerReferencePage, mode, updatedAnswers)
    )

  def onPageLoad(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor) { implicit request =>

      val yesOrNoPage       = UniqueTaxpayerReferenceYesNoPage
      val yesOrNoPageOption = request.userAnswers.get(UniqueTaxpayerReferenceYesNoPage)

      subcontractorNameExtractor
        .getSubcontractorName(request.userAnswers, mode)
        .fold {
          logger.error("[SubcontractorsUniqueTaxpayerReferenceController][onPageLoad] - subcontractor name missing")
          recoveryRedirect
        } { subcontractorName =>
          val result = Ok(view(preparedForm, mode, subcontractorName))
          yesOrNoPageGuardService.yesOrNoPageRoute(result, yesOrNoPageOption, yesOrNoPage, mode)
        }
    }

  def onSubmit(mode: Mode): Action[AnyContent] =
    (identify andThen getData andThen requireData andThen redirectVerifiedSubcontractor).async { implicit request =>
      subcontractorNameExtractor
        .getSubcontractorName(request.userAnswers, mode)
        .fold {
          logger.error("[SubcontractorsUniqueTaxpayerReferenceController][onSubmit] - subcontractor name missing")
          Future.successful(recoveryRedirect)
        } { subcontractorName =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, mode, subcontractorName))),
              value =>
                val prevValue = request.userAnswers.get(SubcontractorsUniqueTaxpayerReferencePage)
                val baseValue = request.userAnswers.get(FinalValidationBaseUtrPage)

                mode match {
                  case AmendMode if prevValue.contains(value) =>
                    saveAndContinue(mode, value)

                  case FinalValidationMode if prevValue.contains(value) || baseValue.contains(value) =>
                    saveAndContinue(mode, value)

                  case _ =>
                    subcontractorService.isDuplicateUTR(request.userAnswers, value).flatMap {
                      case true =>
                        val errorForm = form
                          .fill(value)
                          .withError(
                            key = "value",
                            message = "subcontractorsUniqueTaxpayerReference.error.duplicate"
                          )

                        Future.successful(
                          BadRequest(view(errorForm, mode, subcontractorName))
                        )

                      case false =>
                        saveAndContinue(mode, value)
                    }
                }
            )
        }
    }
}
