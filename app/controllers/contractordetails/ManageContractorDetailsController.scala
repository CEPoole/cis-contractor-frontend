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

package controllers.contractordetails

import controllers.AgentClientChecks
import controllers.actions.*
import controllers.helpers.ContractorDetailsPopulator
import models.{Scheme, UserAnswers}
import pages.contractordetails.{ContractorDetailsValidationTargetPage, ContractorSchemePage}
import play.api.Logging
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import queries.CisIdQuery
import repositories.SessionRepository
import services.{CisManageService, ContractorDetailsService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class ManageContractorDetailsController @Inject() (
  override val messagesApi: MessagesApi,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  contractorDetailsService: ContractorDetailsService,
  override protected val cisManageService: CisManageService,
  override protected val sessionRepository: SessionRepository,
  val controllerComponents: MessagesControllerComponents
)(implicit
  ec: ExecutionContext
) extends FrontendBaseController
    with I18nSupport
    with Logging
    with AgentClientChecks {

  def onPageLoad(target: Option[String] = None): Action[AnyContent] =
    (identify andThen getData).async { implicit request =>

      val userAnswers =
        request.userAnswers.getOrElse(UserAnswers(request.userId))

      withAgentClientChecks(
        request.userId,
        request.isAgent,
        userAnswers
      ).flatMap {

        case Left(redirect) =>
          Future.successful(redirect)

        case Right(checkedAnswers) =>
          Future
            .fromTry(checkedAnswers.remove(ContractorDetailsValidationTargetPage))
            .flatMap { cleanedAnswers =>
              getCisId(cleanedAnswers).flatMap { cisId =>
                contractorDetailsService
                  .getScheme(cisId)
                  .flatMap { scheme =>
                    cleanedAnswers
                      .set(ContractorSchemePage, scheme) match {

                      case Failure(error) =>
                        Future.failed(error)

                      case Success(answersWithScheme) =>
                        if (shouldRedirectToCheckAnswers(scheme)) {

                          ContractorDetailsPopulator
                            .populate(
                              answersWithScheme,
                              scheme
                            )
                            .fold(
                              Future.failed,
                              updatedAnswers =>
                                sessionRepository
                                  .set(updatedAnswers)
                                  .map(_ =>
                                    Redirect(
                                      routes.ContractorDetailsCheckAnswersController.onPageLoad(target)
                                    )
                                  )
                            )
                        } else {
                          sessionRepository
                            .set(answersWithScheme)
                            .map(_ =>
                              Redirect(
                                routes.ContractorDetailsController.onPageLoad()
                              )
                            )
                        }
                    }
                  }
              }
            }
      }.recover { case error =>
        logger.error(
          "[ManageContractorDetailsController] Failed to retrieve contractor details",
          error
        )

        Redirect(controllers.routes.JourneyRecoveryController.onPageLoad())
      }
    }

  private def getCisId(
    userAnswers: UserAnswers
  ): Future[String] =
    userAnswers.get(CisIdQuery) match {
      case Some(cisId) =>
        Future.successful(cisId)

      case None =>
        Future.failed(
          new RuntimeException(
            "CisIdQuery not found in session data"
          )
        )
    }

  private def shouldRedirectToCheckAnswers(
    scheme: Scheme
  ): Boolean =
    scheme.utr.exists(_.trim.nonEmpty)

}
