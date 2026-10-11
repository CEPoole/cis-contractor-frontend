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

package controllers.amend.trust

import controllers.actions.*
import controllers.amend.AmendControllerUtils
import controllers.helpers.SubcontractorNameDisplayHelper
import forms.amend.trust.AmendTrustRemoveDetailYesNoFormProvider
import models.amend.trust.AmendTrustRemoveDetail
import models.{AmendMode, UserAnswers}
import pages.add.trust.*
import pages.amend.trust.AmendTrustRemoveDetailYesNoPage
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.amend.trust.AmendTrustRemoveDetailYesNoView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class AmendTrustRemoveDetailYesNoController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: AmendTrustRemoveDetailYesNoFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: AmendTrustRemoveDetailYesNoView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with Logging {

  private def withValidDetail(
    detail: String
  )(
    action: AmendTrustRemoveDetail => Future[Result]
  ): Future[Result] =
    AmendTrustRemoveDetail.fromKey(detail) match {

      case Some(detailType) =>
        action(detailType)

      case None =>
        logger.error(s"[AmendTrustRemoveDetailYesNoController][withValidDetail] - invalid detail=$detail")
        Future.successful(
          Redirect(
            controllers.routes.JourneyRecoveryController.onPageLoad()
          )
        )
    }

  private def detailIsPresent(
    detail: AmendTrustRemoveDetail,
    userAnswers: UserAnswers
  ): Boolean =
    detail match {

      case AmendTrustRemoveDetail.Address =>
        userAnswers
          .get(TrustAddressYesNoPage)
          .contains(true)

      case AmendTrustRemoveDetail.ContactDetails =>
        userAnswers
          .get(AddTrustContactMethodsYesNoPage)
          .contains(true)

      case AmendTrustRemoveDetail.Utr =>
        userAnswers
          .get(TrustUtrYesNoPage)
          .contains(true) &&
        !AmendControllerUtils.isVerifiedForAmendJourney(userAnswers)

      case AmendTrustRemoveDetail.WorksReferenceNumber =>
        userAnswers
          .get(TrustWorksReferenceYesNoPage)
          .contains(true)
    }

  private def journeyRecovery: Result =
    Redirect(
      controllers.routes.JourneyRecoveryController.onPageLoad()
    )

  def onPageLoad(subcontractorDetail: String): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>
      SubcontractorNameDisplayHelper
        .getTrustDisplayName(request.userAnswers, AmendMode)
        .map { trustName =>
          withValidDetail(subcontractorDetail) { detailType =>
            if (!detailIsPresent(detailType, request.userAnswers)) {
              logger.error(
                s"[AmendTrustRemoveDetailYesNoController][onPageLoad] - detail missing, detail=$subcontractorDetail"
              )

              Future.successful(journeyRecovery)

            } else {
              val form = formProvider()

              val messages =
                messagesApi.preferred(request)

              val subcontractorDetailTitle =
                messages(detailType.messageKey)

              Future.successful(Ok(view(trustName, subcontractorDetail, subcontractorDetailTitle, form)))
            }
          }
        }
        .getOrElse {
          logger.error("[AmendTrustRemoveDetailYesNoController][onPageLoad] - trust name missing from userAnswers")
          Future.successful(journeyRecovery)
        }
    }

  def onSubmit(subcontractorDetail: String): Action[AnyContent] = (identify andThen getData andThen requireData).async {
    implicit request =>
      SubcontractorNameDisplayHelper
        .getTrustDisplayName(request.userAnswers, AmendMode)
        .map { trustName =>
          withValidDetail(subcontractorDetail) { detailType =>
            if (!detailIsPresent(detailType, request.userAnswers)) {
              logger.error(
                s"[AmendTrustRemoveDetailYesNoController][onSubmit] - detail missing, detail=$subcontractorDetail"
              )

              Future.successful(journeyRecovery)

            } else {
              formProvider()
                .bindFromRequest()
                .fold(
                  formWithErrors =>
                    val messages =
                      messagesApi.preferred(request)

                    val subcontractorDetailTitle =
                      messages(detailType.messageKey)

                    Future.successful(
                      BadRequest(view(trustName, subcontractorDetail, subcontractorDetailTitle, formWithErrors))
                    )
                  ,
                  value =>
                    (for {
                      updatedAnswers <-
                        Future.fromTry(
                          request.userAnswers
                            .set(AmendTrustRemoveDetailYesNoPage(detailType), value)
                            .flatMap(_.remove(AmendTrustRemoveDetailYesNoPage(detailType)))
                        )
                      _              <- sessionRepository.set(updatedAnswers)
                    } yield Redirect(
                      controllers.amend.trust.routes.AmendTrustCheckYourAnswersController.onPageLoad().url
                    )).recover { case ex =>
                      logger.error(
                        s"Failed to save remove detail answer for '$subcontractorDetail'",
                        ex
                      )
                      journeyRecovery
                    }
                )
            }
          }
        }
        .getOrElse {
          logger.error("[AmendTrustRemoveDetailYesNoController][onSubmit] - trust name missing from userAnswers")
          Future.successful(journeyRecovery)
        }
  }
}
