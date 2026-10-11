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

package controllers.verify

import controllers.actions.*
import controllers.AgentClientChecks
import forms.verify.SelectSubcontractorFormProvider
import models.{Mode, Subcontractor, SubcontractorViewModel, UserAnswers}
import navigation.Navigator
import pages.verify.{NewestVerificationBatchResponsePage, SelectSubcontractorPage, UnverifiedSubcontractorsPage}
import play.api.data.Form
import play.api.i18n.{I18nSupport, MessagesApi}
import models.CheckMode
import models.finalvalidation.{FinalValidationContext, VerifyFinalValidationSource}
import pages.finalvalidation.*
import pages.verify.RebuildVerificationFromWarningPage
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request, Result}
import repositories.SessionRepository
import services.{CheckboxPaginationResult, CisManageService, PaginationService, VerificationPreSelectionService, VerificationService}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendBaseController
import views.html.verify.SelectSubcontractorView

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

class SelectSubcontractorController @Inject() (
  override val messagesApi: MessagesApi,
  override protected val sessionRepository: SessionRepository,
  navigator: Navigator,
  identify: IdentifierAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SelectSubcontractorFormProvider,
  paginationService: PaginationService,
  verificationPreSelectionService: VerificationPreSelectionService,
  verificationService: VerificationService,
  override protected val cisManageService: CisManageService,
  val controllerComponents: MessagesControllerComponents,
  view: SelectSubcontractorView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with AgentClientChecks
    with PendingVerificationRequestGuard {

  private val form = formProvider()

  def onPageLoad(mode: Mode, page: Int = 1): Action[AnyContent] =
    (identify andThen getData).async { implicit request =>
      request.userAnswers match {

        case Some(userAnswers) =>
          renderSelectSubcontractorPage(
            userAnswers,
            mode,
            page
          )

        case None =>
          val userAnswers = UserAnswers(request.userId)

          withAgentClientChecks(
            request.userId,
            request.isAgent,
            userAnswers
          ).flatMap {

            case Left(redirect) =>
              Future.successful(redirect)

            case Right(checkedAnswers) =>
              verificationService
                .refreshNewestVerificationBatch(checkedAnswers)
                .flatMap { updatedAnswers =>
                  renderSelectSubcontractorPage(
                    updatedAnswers,
                    mode,
                    page
                  )
                }
          }.recover { case t =>
            logger.error(
              "[SelectSubcontractorController.onPageLoad] Failed to initialise verification data",
              t
            )

            Redirect(
              controllers.routes.JourneyRecoveryController.onPageLoad()
            )
          }
      }
    }

  private def renderSelectSubcontractorPage(
    userAnswers: UserAnswers,
    mode: Mode,
    page: Int
  )(implicit request: Request[_]): Future[Result] =
    redirectIfVerificationRequestInProgress(userAnswers) match {
      case Some(redirectResult) =>
        Future.successful(redirectResult)

      case None =>
        getUnverifiedSubcontractorsOrRedirect(userAnswers) match {

          case Right(unverifiedSubcontractors) =>
            val subcontractorsVm =
              SubcontractorViewModel.fromSubcontractors(unverifiedSubcontractors)

            val selectedSubcontractors =
              userAnswers.get(SelectSubcontractorPage) match {

                case Some(subs) =>
                  Future.successful(subs)

                case None =>
                  val selectedIds =
                    verificationPreSelectionService.preSelectedSubcontractorIds(
                      unverifiedSubcontractors,
                      userAnswers
                    )

                  val defaultSelections =
                    subcontractorsVm
                      .filter(sub => selectedIds.contains(sub.id))
                      .toSet

                  Future
                    .fromTry(
                      userAnswers.set(
                        SelectSubcontractorPage,
                        defaultSelections
                      )
                    )
                    .flatMap { updatedAnswers =>
                      sessionRepository
                        .set(updatedAnswers)
                        .map(_ => defaultSelections)
                    }
              }

            val result =
              paginationService.paginateCheckboxItems(
                SubcontractorViewModel.checkboxItems(subcontractorsVm),
                page
              )

            selectedSubcontractors.map { selected =>
              Ok(
                view(
                  form.fill(selected.map(_.id)),
                  mode,
                  result.paginatedData,
                  result.paginationViewModel,
                  result.currentPage,
                  result.startIndex,
                  result.totalCount,
                  result.totalPages
                )
              )
            }

          case Left(redirectResult) =>
            Future.successful(redirectResult)
        }
    }

  private def hasAnyVerifiedSubcontractor(
    userAnswers: UserAnswers
  ): Boolean =
    userAnswers
      .get(NewestVerificationBatchResponsePage)
      .exists(_.subcontractors.exists(_.isVerified))

  def onSubmit(mode: Mode, page: Int = 1): Action[AnyContent] =
    (identify andThen getData andThen requireData).async { implicit request =>

      val ua = request.userAnswers

      redirectIfVerificationRequestInProgress(ua) match {
        case Some(redirectResult) =>
          Future.successful(redirectResult)

        case None =>
          ua.get(UnverifiedSubcontractorsPage) match {

            case None =>
              logger.error(
                "[SelectSubcontractorController][onSubmit] - UnverifiedSubcontractorsPage missing from userAnswers"
              )
              Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))

            case Some(unverifiedSubcontractors) if unverifiedSubcontractors.isEmpty =>
              logger.error("[SelectSubcontractorController][onSubmit] - UnverifiedSubcontractorsPage is empty")
              Future.successful(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))

            case Some(unverifiedSubcontractors) =>
              val subcontractorsVm =
                SubcontractorViewModel.fromSubcontractors(unverifiedSubcontractors)

              val allItems = SubcontractorViewModel.checkboxItems(subcontractorsVm)

              val result =
                paginationService.paginateCheckboxItems(allItems, page)

              val currentPageIds: Set[String] =
                result.paginatedData.map(_.value).toSet

              val otherPageValues: Set[SubcontractorViewModel] =
                ua.get(SelectSubcontractorPage)
                  .getOrElse(Set.empty)
                  .filterNot(sub => currentPageIds.contains(sub.id))

              val boundForm = form.bindFromRequest()

              val currentSelectedValues: Set[SubcontractorViewModel] =
                boundForm.value
                  .getOrElse(Set.empty)
                  .flatMap(id => subcontractorsVm.find(_.id == id))

              val mergedValues: Set[SubcontractorViewModel] =
                otherPageValues ++ currentSelectedValues

              val gotoPage: Option[Int] =
                request.body.asFormUrlEncoded
                  .flatMap(_.get("gotoPage"))
                  .flatMap(_.headOption)
                  .flatMap(_.toIntOption)

              gotoPage match {
                case Some(targetPage) =>
                  for {
                    updatedAnswers <- Future.fromTry(
                                        ua.set(SelectSubcontractorPage, mergedValues)
                                      )
                    _              <- sessionRepository.set(updatedAnswers)
                  } yield Redirect(
                    routes.SelectSubcontractorController.onPageLoad(mode, targetPage)
                  )

                case None =>
                  if (mergedValues.size > 100) {
                    val formWithErrors =
                      form
                        .fill(currentSelectedValues.map(_.id))
                        .withError(
                          "value",
                          "verify.selectSubcontractor.error.maxSelected"
                        )

                    Future.successful(
                      renderPageWithError(formWithErrors, mode, result)
                    )
                  } else if (mergedValues.nonEmpty || hasAnyVerifiedSubcontractor(ua)) {
                    for {
                      answersWithSelections <- Future.fromTry(
                                                 ua.set(SelectSubcontractorPage, mergedValues)
                                               )
                      cleanedAnswers        <-
                        if (
                          mode == CheckMode &&
                          answersWithSelections
                            .get(RebuildVerificationFromWarningPage)
                            .contains(true)
                        ) {
                          Future.fromTry(
                            answersWithSelections.remove(RebuildVerificationFromWarningPage)
                          )
                        } else {
                          Future.successful(answersWithSelections)
                        }

                      withContext <- Future.fromTry(
                                       cleanedAnswers.set(
                                         FinalValidationContextPage,
                                         FinalValidationContext.VerifySubcontractor
                                       )
                                     )

                      withSource <- Future.fromTry(
                                      withContext.set(
                                        VerifyFinalValidationSourcePage,
                                        VerifyFinalValidationSource.SelectSubcontractor
                                      )
                                    )
                      _          <- sessionRepository.set(withSource)
                    } yield Redirect(
                      navigator.nextPage(
                        SelectSubcontractorPage,
                        mode,
                        withSource
                      )
                    )
                  } else {
                    val formWithErrors =
                      form
                        .fill(currentSelectedValues.map(_.id))
                        .withError("value", "verify.selectSubcontractor.error.required")

                    Future.successful(renderPageWithError(formWithErrors, mode, result))
                  }
              }
          }
      }
    }

  private def renderPageWithError(
    formWithErrors: Form[Set[String]],
    mode: Mode,
    result: CheckboxPaginationResult
  )(implicit request: Request[_]): Result =
    BadRequest(
      view(
        formWithErrors,
        mode,
        result.paginatedData,
        result.paginationViewModel,
        result.currentPage,
        result.startIndex,
        result.totalCount,
        result.totalPages
      )
    )

  private def getUnverifiedSubcontractorsOrRedirect(userAnswers: UserAnswers): Either[Result, Seq[Subcontractor]] =
    userAnswers.get(NewestVerificationBatchResponsePage) match {
      case None =>
        logger.error(
          "[SelectSubcontractorController][getUnverifiedSubcontractorsOrRedirect] - " +
            "NewestVerificationBatchResponsePage missing from userAnswers"
        )
        Left(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))

      case Some(newestVerificationBatchResponse) if newestVerificationBatchResponse.subcontractors.isEmpty =>
        Left(Redirect(routes.NoSubcontractorsAddedController.onPageLoad()))

      case Some(_) =>
        userAnswers.get(UnverifiedSubcontractorsPage) match {
          case Some(unverifiedSubcontractors) if unverifiedSubcontractors.nonEmpty =>
            Right(unverifiedSubcontractors)

          case Some(_) =>
            Left(Redirect(controllers.verify.routes.VerifyYourSubcontractorsYesNoController.onPageLoad))

          case None =>
            logger.error(
              "[SelectSubcontractorController][getUnverifiedSubcontractorsOrRedirect] - " +
                "UnverifiedSubcontractorsPage missing from userAnswers"
            )
            Left(Redirect(controllers.routes.JourneyRecoveryController.onPageLoad()))
        }
    }
}
