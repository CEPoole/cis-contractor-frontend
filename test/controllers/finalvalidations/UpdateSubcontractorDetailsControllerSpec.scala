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
import models.UserAnswers
import models.finalvalidation.*
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, verify, verifyNoInteractions, when}
import org.scalatestplus.mockito.MockitoSugar.mock
import pages.finalvalidation.FinalValidationDraftIdPage
import play.api.inject.bind
import play.api.libs.json.Json
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import play.api.test.CSRFTokenHelper
import services.VerifyFinalValidationService
import services.finalvalidation.FinalValidationDraftService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.util.Success

class UpdateSubcontractorDetailsControllerSpec extends SpecBase {

  private val draftId = "draft-123"

  private val subcontractorId = 10903L

  private def updateSubcontractorDetailsRoute =
    controllers.finalvalidations.routes.UpdateSubcontractorDetailsController
      .onPageLoad(subcontractorId)
      .url

  private def submitSubcontractorDetailsRoute =
    controllers.finalvalidations.routes.UpdateSubcontractorDetailsController
      .onSubmit(subcontractorId)
      .url

  private def reviewSubcontractorDetailsRoute =
    controllers.finalvalidations.routes.ReviewSubcontractorDetailsController
      .onPageLoad()
      .url

  private def cancelSubcontractorDetailsRoute =
    controllers.finalvalidations.routes.UpdateSubcontractorDetailsController
      .onCancel(subcontractorId)
      .url

  private val userAnswers =
    userAnswersWithCisId
      .setOrException(
        FinalValidationDraftIdPage,
        draftId
      )

  private def draft(
    readiness: String = "Incomplete",
    id: Long = subcontractorId
  ): FinalValidationDraft =
    Json
      .obj(
        "subcontractors" -> Json.arr(
          Json.obj(
            "subcontractorId"   -> id,
            "subbieResourceRef" -> 7L,
            "baseVersion"       -> 12,
            "subcontractorType" -> "soletrader",
            "displayName"       -> "Hooper And Associates",
            "base"              -> Json.obj(
              "firstName" -> "A",
              "surname"   -> "Alice",
              "utr"       -> "1111111111",
              "nino"      -> "PX123456A"
            ),
            "proposed"          -> Json.obj(
              "firstName" -> "A",
              "surname"   -> "Alice",
              "utr"       -> "2234567890",
              "nino"      -> "PX123456A"
            ),
            "changedTargets"    -> Json.arr("utr"),
            "issues"            -> Json.arr(),
            "readiness"         -> readiness
          )
        )
      )
      .as[FinalValidationDraft]

  "UpdateSubcontractorDetailsController.onPageLoad" - {

    "must return OK for an incomplete subcontractor" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft()
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(userAnswers),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          CSRFTokenHelper.addCSRFToken(
            FakeRequest(
              GET,
              updateSubcontractorDetailsRoute
            )
          )

        val result =
          route(application, request).value

        status(result) mustBe OK

        contentAsString(result) must include(
          "A Alice"
        )

        verify(finalValidationDraftService)
          .get(
            any[String],
            any[String]
          )(any[HeaderCarrier])

        verifyNoInteractions(
          verifyFinalValidationService
        )
      }
    }

    "must redirect to Review Subcontractor Details when the subcontractor is complete" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            readiness = "Complete"
          )
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(userAnswers),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            updateSubcontractorDetailsRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          reviewSubcontractorDetailsRoute

        verifyNoInteractions(
          verifyFinalValidationService
        )
      }
    }

    "must redirect to Journey Recovery when the subcontractor is not in the draft" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            id = 99999L
          )
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(userAnswers),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            updateSubcontractorDetailsRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          verifyFinalValidationService
        )
      }
    }

    "must redirect to Journey Recovery when the draft id is missing" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      val application =
        applicationWith(
          userAnswers = Some(userAnswersWithCisId),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            updateSubcontractorDetailsRoute
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
          verifyFinalValidationService
        )
      }
    }

    "must redirect to Journey Recovery when no UserAnswers exist" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      val application =
        applicationWith(
          userAnswers = None,
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            GET,
            updateSubcontractorDetailsRoute
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
          verifyFinalValidationService
        )
      }
    }
  }

  "UpdateSubcontractorDetailsController.onSubmit" - {

    "must validate the draft subcontractor, update readiness and redirect to Review Subcontractor Details" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      val incompleteDraft =
        draft()

      val issues =
        Seq.empty[FinalValidationDraftIssue]

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          incompleteDraft
        )
      )

      when(
        verifyFinalValidationService.validateDraftSubcontractor(
          incompleteDraft,
          subcontractorId
        )
      ).thenReturn(
        Success(issues)
      )

      when(
        finalValidationDraftService.updateReadiness(
          any[String],
          any[String],
          any[Long],
          any[Seq[FinalValidationDraftIssue]]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          incompleteDraft
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(userAnswers),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitSubcontractorDetailsRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          reviewSubcontractorDetailsRoute

        verify(verifyFinalValidationService)
          .validateDraftSubcontractor(
            incompleteDraft,
            subcontractorId
          )

        verify(finalValidationDraftService)
          .updateReadiness(
            any[String],
            any[String],
            any[Long],
            any[Seq[FinalValidationDraftIssue]]
          )(any[HeaderCarrier])
      }
    }

    "must redirect to Review Subcontractor Details without validating when the subcontractor is already complete" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            readiness = "Complete"
          )
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(userAnswers),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitSubcontractorDetailsRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          reviewSubcontractorDetailsRoute

        verifyNoInteractions(
          verifyFinalValidationService
        )

        verify(
          finalValidationDraftService,
          never()
        ).updateReadiness(
          any[String],
          any[String],
          any[Long],
          any[Seq[FinalValidationDraftIssue]]
        )(any[HeaderCarrier])
      }
    }

    "must redirect to Journey Recovery when the subcontractor is not in the draft" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      when(
        finalValidationDraftService.get(
          any[String],
          any[String]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          draft(
            id = 99999L
          )
        )
      )

      val application =
        applicationWith(
          userAnswers = Some(userAnswers),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitSubcontractorDetailsRoute
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verifyNoInteractions(
          verifyFinalValidationService
        )

        verify(
          finalValidationDraftService,
          never()
        ).updateReadiness(
          any[String],
          any[String],
          any[Long],
          any[Seq[FinalValidationDraftIssue]]
        )(any[HeaderCarrier])
      }
    }

    "must redirect to Journey Recovery when the draft id is missing" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      val application =
        applicationWith(
          userAnswers = Some(userAnswersWithCisId),
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitSubcontractorDetailsRoute
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
          verifyFinalValidationService
        )
      }
    }

    "must redirect to Journey Recovery when no UserAnswers exist" in {
      val finalValidationDraftService =
        mock[FinalValidationDraftService]

      val verifyFinalValidationService =
        mock[VerifyFinalValidationService]

      val application =
        applicationWith(
          userAnswers = None,
          finalValidationDraftService = finalValidationDraftService,
          verifyFinalValidationService = verifyFinalValidationService
        )

      running(application) {
        val request =
          FakeRequest(
            POST,
            submitSubcontractorDetailsRoute
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
          verifyFinalValidationService
        )
      }
    }

    "UpdateSubcontractorDetailsController.onCancel" - {

      "must reset the subcontractor and redirect to Review Subcontractor Details" in {
        val finalValidationDraftService =
          mock[FinalValidationDraftService]

        val verifyFinalValidationService =
          mock[VerifyFinalValidationService]

        when(
          finalValidationDraftService.resetSubcontractor(
            any[String],
            any[String],
            any[Long]
          )(any[HeaderCarrier])
        ).thenReturn(
          Future.successful(
            draft()
          )
        )

        val application =
          applicationWith(
            userAnswers = Some(userAnswers),
            finalValidationDraftService = finalValidationDraftService,
            verifyFinalValidationService = verifyFinalValidationService
          )

        running(application) {
          val request =
            FakeRequest(
              POST,
              cancelSubcontractorDetailsRoute
            )

          val result =
            route(application, request).value

          status(result) mustBe SEE_OTHER

          redirectLocation(result).value mustBe
            reviewSubcontractorDetailsRoute

          verify(finalValidationDraftService)
            .resetSubcontractor(
              any[String],
              any[String],
              any[Long]
            )(any[HeaderCarrier])

          verifyNoInteractions(
            verifyFinalValidationService
          )
        }
      }

      "must redirect to Journey Recovery when the draft id is missing" in {
        val finalValidationDraftService =
          mock[FinalValidationDraftService]

        val verifyFinalValidationService =
          mock[VerifyFinalValidationService]

        val application =
          applicationWith(
            userAnswers = Some(userAnswersWithCisId),
            finalValidationDraftService = finalValidationDraftService,
            verifyFinalValidationService = verifyFinalValidationService
          )

        running(application) {
          val request =
            FakeRequest(
              POST,
              cancelSubcontractorDetailsRoute
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
            verifyFinalValidationService
          )
        }
      }

      "must redirect to Journey Recovery when no UserAnswers exist" in {
        val finalValidationDraftService =
          mock[FinalValidationDraftService]

        val verifyFinalValidationService =
          mock[VerifyFinalValidationService]

        val application =
          applicationWith(
            userAnswers = None,
            finalValidationDraftService = finalValidationDraftService,
            verifyFinalValidationService = verifyFinalValidationService
          )

        running(application) {
          val request =
            FakeRequest(
              POST,
              cancelSubcontractorDetailsRoute
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
            verifyFinalValidationService
          )
        }
      }
    }
  }

  private def applicationWith(
    userAnswers: Option[UserAnswers],
    finalValidationDraftService: FinalValidationDraftService,
    verifyFinalValidationService: VerifyFinalValidationService
  ) =
    applicationBuilder(userAnswers = userAnswers)
      .configure(
        "play.http.context" -> "/"
      )
      .overrides(
        bind[FinalValidationDraftService]
          .toInstance(finalValidationDraftService),
        bind[VerifyFinalValidationService]
          .toInstance(verifyFinalValidationService)
      )
      .build()
}
