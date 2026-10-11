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

package controllers.finalvalidations

import base.SpecBase
import models.{CheckMode, NormalMode, SubcontractorViewModel, UserAnswers}
import models.finalvalidation.*
import models.RichJsObject
import models.verify.SelectedSubcontractors
import pages.verify.{SelectSubcontractorPage, SelectSubcontractorsToReverifyPage, VerificationBatchReadinessPage}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{verify, verifyNoInteractions, when}
import org.scalatestplus.mockito.MockitoSugar.mock
import pages.finalvalidation.*
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import services.finalvalidation.FinalValidationDraftService
import uk.gov.hmrc.http.HeaderCarrier
import views.html.finalvalidations.ReviewSubcontractorDetailsView

import scala.concurrent.Future

class ReviewSubcontractorDetailsControllerSpec extends SpecBase {

  private val cisId   = "1"
  private val draftId = "draft-id"

  private val payload =
    FinalValidationHandoffPayload(
      draftId = draftId,
      instanceId = cisId,
      subcontractorId = 1L,
      subbieResourceRef = 10L,
      field = FinalValidationField.Utr,
      changeTarget = FinalValidationChangeTarget.Utr
    )

  private def reviewRoute =
    routes.ReviewSubcontractorDetailsController.onPageLoad().url

  private def submitRoute =
    routes.ReviewSubcontractorDetailsController.onSubmit.url

  private def userAnswers(
    source: VerifyFinalValidationSource,
    mode: String = "NormalMode"
  ): UserAnswers =
    userAnswersWithCisId
      .setOrException(
        FinalValidationDraftIdPage,
        draftId
      )
      .setOrException(
        VerifyFinalValidationSourcePage,
        source
      )
      .setOrException(
        VerifyFinalValidationModePage,
        mode
      )

  private def userAnswersForCommit(
    source: VerifyFinalValidationSource,
    continuation: String,
    mode: String = "NormalMode"
  ): UserAnswers =
    userAnswers(source, mode)
      .setOrException(
        FinalValidationContextPage,
        FinalValidationContext.VerifySubcontractor
      )
      .setOrException(
        VerifyFinalValidationPayloadPage,
        payload
      )
      .setOrException(
        FinalValidationChangeTargetPage,
        FinalValidationChangeTarget.Utr
      )
      .setOrException(
        VerifyFinalValidationContinuationPage,
        continuation
      )

  private def answersWithCorrectedName(
    answers: UserAnswers,
    name: String
  ): UserAnswers = {

    val selectedSubcontractors =
      answers
        .get(SelectSubcontractorPage)
        .value
        .map { subcontractor =>
          if (subcontractor.id == "1") {
            subcontractor.copy(name = name)
          } else {
            subcontractor
          }
        }

    val updatedData =
      answers.data
        .setObject(
          SelectSubcontractorPage.path,
          Json.toJson(selectedSubcontractors)
        )
        .get

    answers.copy(
      data = updatedData
    )
  }

  private def answersWithCorrectedReverifyName(
    answers: UserAnswers,
    name: String
  ): UserAnswers = {

    val selectedSubcontractors =
      answers
        .get(SelectSubcontractorsToReverifyPage)
        .value
        .map { subcontractor =>
          if (subcontractor.id == "1") {
            subcontractor.copy(name = name)
          } else {
            subcontractor
          }
        }

    val updatedData =
      answers.data
        .setObject(
          SelectSubcontractorsToReverifyPage.path,
          Json.toJson(selectedSubcontractors)
        )
        .get

    answers.copy(
      data = updatedData
    )
  }

  private def cleanedAnswers(
    answers: UserAnswers
  ): UserAnswers =
    answers
      .remove(FinalValidationDraftIdPage)
      .success
      .value
      .remove(VerifyFinalValidationSourcePage)
      .success
      .value
      .remove(VerifyFinalValidationModePage)
      .success
      .value
      .remove(FinalValidationContextPage)
      .success
      .value
      .remove(VerifyFinalValidationPayloadPage)
      .success
      .value
      .remove(FinalValidationChangeTargetPage)
      .success
      .value
      .remove(FinalValidationBaseUtrPage)
      .success
      .value
      .remove(VerifyFinalValidationContinuationPage)
      .success
      .value

  private def draft(
    firstReadiness: String,
    secondReadiness: String
  ): FinalValidationDraft =
    Json
      .obj(
        "subcontractors" -> Json.arr(
          Json.obj(
            "subcontractorId"   -> 1L,
            "subbieResourceRef" -> 10L,
            "baseVersion"       -> 1,
            "subcontractorType" -> "soletrader",
            "displayName"       -> "First Subcontractor",
            "base"              -> Json.obj(
              "firstName" -> "First",
              "surname"   -> "Subcontractor"
            ),
            "proposed"          -> Json.obj(
              "firstName" -> "First",
              "surname"   -> "Subcontractor"
            ),
            "changedTargets"    -> Json.arr(),
            "issues"            -> Json.arr(),
            "readiness"         -> firstReadiness
          ),
          Json.obj(
            "subcontractorId"   -> 2L,
            "subbieResourceRef" -> 20L,
            "baseVersion"       -> 1,
            "subcontractorType" -> "soletrader",
            "displayName"       -> "Second Subcontractor",
            "base"              -> Json.obj(
              "firstName" -> "Second",
              "surname"   -> "Subcontractor"
            ),
            "proposed"          -> Json.obj(
              "firstName" -> "Second",
              "surname"   -> "Subcontractor"
            ),
            "changedTargets"    -> Json.arr(),
            "issues"            -> Json.arr(),
            "readiness"         -> secondReadiness
          )
        )
      )
      .as[FinalValidationDraft]

  private def applicationWith(
    userAnswers: Option[UserAnswers],
    finalValidationDraftService: FinalValidationDraftService,
    sessionRepository: SessionRepository
  ) =
    applicationBuilder(userAnswers = userAnswers)
      .configure(
        "play.http.context" -> "/"
      )
      .overrides(
        bind[FinalValidationDraftService]
          .toInstance(finalValidationDraftService),
        bind[SessionRepository]
          .toInstance(sessionRepository)
      )
      .build()

  "ReviewSubcontractorDetailsController.onPageLoad" - {

    "must render the subcontractors from the Final Validation draft" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswers(
          VerifyFinalValidationSource.SelectSubcontractor
        )

      val currentDraft =
        draft(
          firstReadiness = "Incomplete",
          secondReadiness = "Complete"
        )

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(currentDraft)
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            reviewRoute
          )

        val result =
          route(application, request).value

        val view =
          application.injector
            .instanceOf[ReviewSubcontractorDetailsView]

        val expectedModel =
          ReviewSubcontractorDetailsPageModel(
            subcontractors = Seq(
              ReviewSubcontractorDetailsRow(
                subcontractorId = 1L,
                name = "First Subcontractor",
                hasErrors = true
              ),
              ReviewSubcontractorDetailsRow(
                subcontractorId = 2L,
                name = "Second Subcontractor",
                hasErrors = false
              )
            ),
            canContinue = false,
            backUrl = controllers.verify.routes.SelectSubcontractorController
              .onPageLoad(NormalMode)
              .url
          )

        status(result) mustBe OK

        contentAsString(result) mustBe
          view(expectedModel)(
            request,
            messages(application)
          ).toString

        verify(finalValidationDraftService)
          .get(
            any[String],
            any[String]
          )(any[HeaderCarrier])
      }
    }

    "must use SelectSubcontractorsToReverify as the back page when that was the source" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswers(
          source = VerifyFinalValidationSource.SelectSubcontractorsToReverify,
          mode = "CheckMode"
        )

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            firstReadiness = "Incomplete",
            secondReadiness = "Complete"
          )
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            reviewRoute
          )

        val result =
          route(application, request).value

        val view =
          application.injector
            .instanceOf[ReviewSubcontractorDetailsView]

        val expectedModel =
          ReviewSubcontractorDetailsPageModel(
            subcontractors = Seq(
              ReviewSubcontractorDetailsRow(
                subcontractorId = 1L,
                name = "First Subcontractor",
                hasErrors = true
              ),
              ReviewSubcontractorDetailsRow(
                subcontractorId = 2L,
                name = "Second Subcontractor",
                hasErrors = false
              )
            ),
            canContinue = false,
            backUrl = controllers.verify.routes.SelectSubcontractorsToReverifyController
              .onPageLoad(CheckMode)
              .url
          )

        status(result) mustBe OK

        contentAsString(result) mustBe
          view(expectedModel)(
            request,
            messages(application)
          ).toString
      }
    }

    "must redirect to Journey Recovery when the draft id is missing" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersWithCisId
          .setOrException(
            VerifyFinalValidationSourcePage,
            VerifyFinalValidationSource.SelectSubcontractor
          )
          .setOrException(
            VerifyFinalValidationModePage,
            "NormalMode"
          )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            reviewRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          finalValidationDraftService
        )
      }
    }

    "must redirect to Journey Recovery when the source is missing" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersWithCisId
          .setOrException(
            FinalValidationDraftIdPage,
            draftId
          )
          .setOrException(
            VerifyFinalValidationModePage,
            "NormalMode"
          )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            reviewRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          finalValidationDraftService
        )
      }
    }

    "must redirect to Journey Recovery when the mode is invalid" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswers(
          source = VerifyFinalValidationSource.SelectSubcontractor,
          mode = "InvalidMode"
        )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            reviewRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          finalValidationDraftService
        )
      }
    }

    "must redirect to Journey Recovery when there are no UserAnswers" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val application =
        applicationWith(
          userAnswers = None,
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            reviewRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url
      }
    }
  }

  "ReviewSubcontractorDetailsController.onSubmit" - {

    "must redirect back to Review Subcontractor Details when the draft is incomplete" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersForCommit(
          source = VerifyFinalValidationSource.SelectSubcontractor,
          continuation = VerifyFinalValidationContinuation.ContractorEmailConfirmationStored
        )

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            firstReadiness = "Incomplete",
            secondReadiness = "Complete"
          )
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          routes.ReviewSubcontractorDetailsController
            .onPageLoad()
            .url

        verifyNoInteractions(
          sessionRepository
        )
      }
    }

    "must commit, clean the session and continue to ContractorEmailConfirmationStored" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersForCommit(
          source = VerifyFinalValidationSource.SelectSubcontractor,
          continuation = VerifyFinalValidationContinuation.ContractorEmailConfirmationStored,
          mode = "NormalMode"
        )

      val expectedAnswers =
        cleanedAnswers(answers)

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            firstReadiness = "Complete",
            secondReadiness = "Complete"
          )
        )
      )

      when(
        finalValidationDraftService.commit(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(())
      )

      when(
        sessionRepository.set(
          expectedAnswers
        )
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.ContractorEmailConfirmationStoredController
            .onPageLoadAfterFinalValidation(NormalMode)
            .url

        verify(finalValidationDraftService)
          .commit(
            any[String],
            any[String]
          )(any[HeaderCarrier])

        verify(sessionRepository)
          .set(
            expectedAnswers
          )
      }
    }

    "must commit, clean the session and continue to ContractorEmailConfirmationNotStored" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersForCommit(
          source = VerifyFinalValidationSource.SelectSubcontractorsToReverify,
          continuation = VerifyFinalValidationContinuation.ContractorEmailConfirmationNotStored,
          mode = "CheckMode"
        )

      val expectedAnswers =
        cleanedAnswers(answers)

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            firstReadiness = "Complete",
            secondReadiness = "Complete"
          )
        )
      )

      when(
        finalValidationDraftService.commit(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(())
      )

      when(
        sessionRepository.set(
          expectedAnswers
        )
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.ContractorEmailConfirmationNotStoredController
            .onPageLoadAfterFinalValidation(CheckMode)
            .url

        verify(finalValidationDraftService)
          .commit(
            any[String],
            any[String]
          )(any[HeaderCarrier])

        verify(sessionRepository)
          .set(
            expectedAnswers
          )
      }
    }

    "must redirect to Journey Recovery when the continuation is missing" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswers(
          VerifyFinalValidationSource.ReviewUnmatchedSubcontractors
        )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          finalValidationDraftService,
          sessionRepository
        )
      }
    }

    "must redirect to Journey Recovery when the draft id is missing" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersWithCisId
          .setOrException(
            VerifyFinalValidationContinuationPage,
            VerifyFinalValidationContinuation.ContractorEmailConfirmationStored
          )
          .setOrException(
            VerifyFinalValidationModePage,
            "NormalMode"
          )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          finalValidationDraftService,
          sessionRepository
        )
      }
    }

    "must redirect to Journey Recovery when the mode is invalid" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersForCommit(
          source = VerifyFinalValidationSource.SelectSubcontractor,
          continuation = VerifyFinalValidationContinuation.ContractorEmailConfirmationStored,
          mode = "InvalidMode"
        )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          finalValidationDraftService,
          sessionRepository
        )
      }
    }

    "must redirect to Journey Recovery when no UserAnswers exist" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val application =
        applicationWith(
          userAnswers = None,
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url
      }
    }

    "must update the selected subcontractor name and preserve verification batch readiness" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersForCommit(
          source = VerifyFinalValidationSource.SelectSubcontractor,
          continuation = VerifyFinalValidationContinuation.ContractorEmailConfirmationStored,
          mode = "NormalMode"
        )
          .setOrException(
            SelectSubcontractorPage,
            Set(
              SubcontractorViewModel(
                id = "1",
                name = "Old Subcontractor Name"
              )
            )
          )
          .setOrException(
            VerificationBatchReadinessPage,
            true
          )

      val currentDraft =
        Json
          .obj(
            "subcontractors" -> Json.arr(
              Json.obj(
                "subcontractorId"   -> 1L,
                "subbieResourceRef" -> 10L,
                "baseVersion"       -> 1,
                "subcontractorType" -> "soletrader",
                "displayName"       -> "Old Subcontractor Name",
                "base"              -> Json.obj(
                  "firstName" -> "Old",
                  "surname"   -> "Subcontractor Name"
                ),
                "proposed"          -> Json.obj(
                  "firstName" -> "Corrected",
                  "surname"   -> "Subcontractor Name"
                ),
                "changedTargets"    -> Json.arr(),
                "issues"            -> Json.arr(),
                "readiness"         -> "Complete"
              )
            )
          )
          .as[FinalValidationDraft]

      val expectedAnswers =
        cleanedAnswers(
          answersWithCorrectedName(
            answers,
            "Corrected Subcontractor Name"
          )
        )

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(currentDraft)
      )

      when(
        finalValidationDraftService.commit(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(())
      )

      when(
        sessionRepository.set(
          expectedAnswers
        )
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.ContractorEmailConfirmationStoredController
            .onPageLoadAfterFinalValidation(NormalMode)
            .url

        verify(finalValidationDraftService)
          .commit(
            any[String],
            any[String]
          )(any[HeaderCarrier])

        verify(sessionRepository)
          .set(
            expectedAnswers
          )

        expectedAnswers
          .get(SelectSubcontractorPage)
          .value
          .map(_.name) mustBe
          Set("Corrected Subcontractor Name")

        expectedAnswers
          .get(VerificationBatchReadinessPage) mustBe
          Some(true)
      }
    }

    "must update the selected reverify subcontractor name and preserve verification batch readiness" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val sessionRepository =
        mock[SessionRepository]

      val answers =
        userAnswersForCommit(
          source = VerifyFinalValidationSource.SelectSubcontractorsToReverify,
          continuation = VerifyFinalValidationContinuation.ContractorEmailConfirmationNotStored,
          mode = "CheckMode"
        )
          .setOrException(
            SelectSubcontractorsToReverifyPage,
            Set(
              SelectedSubcontractors(
                id = "1",
                name = "Old Subcontractor Name"
              )
            )
          )
          .setOrException(
            VerificationBatchReadinessPage,
            true
          )

      val currentDraft =
        Json
          .obj(
            "subcontractors" -> Json.arr(
              Json.obj(
                "subcontractorId"   -> 1L,
                "subbieResourceRef" -> 10L,
                "baseVersion"       -> 1,
                "subcontractorType" -> "soletrader",
                "displayName"       -> "Old Subcontractor Name",
                "base"              -> Json.obj(
                  "firstName" -> "Old",
                  "surname"   -> "Subcontractor Name"
                ),
                "proposed"          -> Json.obj(
                  "firstName" -> "Corrected",
                  "surname"   -> "Subcontractor Name"
                ),
                "changedTargets"    -> Json.arr(),
                "issues"            -> Json.arr(),
                "readiness"         -> "Complete"
              )
            )
          )
          .as[FinalValidationDraft]

      val expectedAnswers =
        cleanedAnswers(
          answersWithCorrectedReverifyName(
            answers,
            "Corrected Subcontractor Name"
          )
        )

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(currentDraft)
      )

      when(
        finalValidationDraftService.commit(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(())
      )

      when(
        sessionRepository.set(
          expectedAnswers
        )
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers = Some(answers),
          finalValidationDraftService = finalValidationDraftService,
          sessionRepository = sessionRepository
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.ContractorEmailConfirmationNotStoredController
            .onPageLoadAfterFinalValidation(CheckMode)
            .url

        verify(finalValidationDraftService)
          .commit(
            any[String],
            any[String]
          )(any[HeaderCarrier])

        verify(sessionRepository)
          .set(
            expectedAnswers
          )

        expectedAnswers
          .get(SelectSubcontractorsToReverifyPage)
          .value
          .map(_.name) mustBe
          Set("Corrected Subcontractor Name")

        expectedAnswers
          .get(VerificationBatchReadinessPage) mustBe
          Some(true)
      }
    }
  }
}
