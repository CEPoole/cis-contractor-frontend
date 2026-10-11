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

import base.SpecBase
import controllers.actions.FormpRdsReconcileAction
import models.UserAnswers
import models.requests.DataRequest
import models.response.*
import models.verify.GovTalkErrorStatus.{DepartmentalError, FatalError}
import models.verify.{SubmissionStatus, VerificationSubmissionDetails}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, verify, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.verify.VerificationSubmissionDetailsPage
import play.api.i18n.Messages
import play.api.inject.bind
import play.api.mvc.Results.Redirect
import play.api.mvc.{AnyContent, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import services.VerificationService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDateTime
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class SubmissionSendingControllerSpec extends SpecBase with MockitoSugar {

  private lazy val onPageLoadRoute =
    controllers.verify.routes.SubmissionSendingController.onPageLoad().url

  private lazy val onPollRoute =
    controllers.verify.routes.SubmissionSendingController.onPollAndRedirect.url

  private val submissionDetails =
    VerificationSubmissionDetails(
      submissionId = "13602",
      status = "ACCEPTED",
      hmrcMarkGenerated = "hmrc-mark",
      hmrcMarkGgis = None,
      correlationId = Some("corr-id"),
      pollUrl = Some("http://localhost/poll"),
      pollIntervalSeconds = Some(5),
      submittedAt = LocalDateTime.parse("2026-06-15T03:30:52"),
      lastMessageDate = None,
      timedOut = false
    )

  private def userAnswersWithSubmissionDetails: UserAnswers =
    emptyUserAnswers
      .set(VerificationSubmissionDetailsPage, submissionDetails)
      .success
      .value

  private def mockInitialSubmission(
    mockService: VerificationService,
    response: ChrisSubmissionResponse
  ): Unit =
    when(
      mockService.createSubmitAndPersistVerificationSubmission(
        any[DataRequest[AnyContent]],
        any[HeaderCarrier],
        any[Messages]
      )
    ).thenReturn(Future.successful(response))

  private def mockPollResponse(
    mockService: VerificationService,
    response: ChrisPollResponse
  ): Unit = {
    when(
      mockService.isPollDue(any[VerificationSubmissionDetails], any[Int])
    ).thenReturn(true)
    when(
      mockService.pollStatusAndPersist(
        any[UserAnswers],
        any[VerificationSubmissionDetails]
      )(any[HeaderCarrier])
    ).thenReturn(Future.successful(response))
  }

  private def pollResponse(
    status: SubmissionStatus,
    govTalkErrorStatus: Option[models.verify.GovTalkErrorStatus] = None
  ): ChrisPollResponse =
    ChrisPollResponse(
      status = status,
      correlationId = "corr-id",
      pollUrl = None,
      pollInterval = None,
      error = None,
      irMarkReceived = None,
      lastMessageDate = None,
      acceptedTime = None,
      govTalkErrorStatus = govTalkErrorStatus
    )

  private def applicationWith(
    mockService: VerificationService,
    userAnswers: UserAnswers = emptyUserAnswers
  ) =
    applicationBuilder(userAnswers = Some(userAnswers))
      .overrides(bind[VerificationService].toInstance(mockService))
      .build()

  "SubmissionSendingController.onPageLoad" - {

    "must redirect to poll page when initial submission returns ACCEPTED" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "ACCEPTED",
          hmrcMarkGenerated = "hmrc-mark"
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.SubmissionSendingController.onPollAndRedirect.url

        verify(mockService).createSubmitAndPersistVerificationSubmission(
          any[DataRequest[AnyContent]],
          any[HeaderCarrier],
          any[Messages]
        )
      }
    }

    "must redirect to submitted page when initial submission returns SUBMITTED_NO_RECEIPT" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "SUBMITTED_NO_RECEIPT",
          hmrcMarkGenerated = "hmrc-mark"
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerificationRequestSubmittedController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when initial submission returns FATAL_ERROR with error code 3000" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "FATAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "3000",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when initial submission returns FATAL_ERROR with error code 1000" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "FATAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "1000",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when initial submission returns FATAL_ERROR with error code 2005" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "FATAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "2005",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when initial submission returns DEPARTMENTAL_ERROR with error code 3000" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "DEPARTMENTAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = Some(
            DepartmentalError(
              errorCode = Some("3000"),
              errorText = "Departmental error"
            )
          )
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to departmental error page when initial submission returns DEPARTMENTAL_ERROR with error code 3001" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "DEPARTMENTAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = Some(
            DepartmentalError(
              errorCode = Some("3001"),
              errorText = "Departmental error"
            )
          )
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorController
            .onPageLoad()
            .url
      }
    }

    "must redirect to Verification Not Submitted Warning when initial submission returns FATAL_ERROR with another error code" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "FATAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "9999",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerificationNotSubmittedWarningController
            .onPageLoad()
            .url
      }
    }

    "must redirect to Verification Not Submitted Warning when initial submission returns FATAL_ERROR without GovTalk error details" in {
      val mockService = mock[VerificationService]

      mockInitialSubmission(
        mockService,
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "FATAL_ERROR",
          hmrcMarkGenerated = "hmrc-mark",
          govTalkErrorStatus = None
        )
      )

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerificationNotSubmittedWarningController
            .onPageLoad()
            .url
      }
    }

    "must redirect to recovery when service fails" in {
      val mockService = mock[VerificationService]

      when(
        mockService.createSubmitAndPersistVerificationSubmission(
          any[DataRequest[AnyContent]],
          any[HeaderCarrier],
          any[Messages]
        )
      ).thenReturn(Future.failed(new RuntimeException("boom")))

      val application = applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url
      }
    }

    "must run the FORMP/RDS reconciliation before submitting to ChRIS and honour its redirect" in {
      val mockService = mock[VerificationService]

      val redirectingReconcile = new FormpRdsReconcileAction {
        override protected def filter[A](request: DataRequest[A]): Future[Option[Result]] =
          Future.successful(
            Some(Redirect(controllers.routes.UnauthorisedOrganisationAffinityController.onPageLoad()))
          )
        override protected def executionContext: ExecutionContext                         = scala.concurrent.ExecutionContext.Implicits.global
      }

      val application =
        applicationBuilder(
          userAnswers = Some(emptyUserAnswers),
          formpRdsReconcileAction = redirectingReconcile
        )
          .overrides(bind[VerificationService].toInstance(mockService))
          .build()

      running(application) {
        val result = route(application, FakeRequest(GET, onPageLoadRoute)).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value mustBe
          controllers.routes.UnauthorisedOrganisationAffinityController.onPageLoad().url

        verify(mockService, never).createSubmitAndPersistVerificationSubmission(
          any[DataRequest[AnyContent]],
          any[HeaderCarrier],
          any[Messages]
        )
      }
    }
  }

  "SubmissionSendingController.onPollAndRedirect" - {

    "must render polling page with Refresh header when poll returns ACCEPTED" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(SubmissionStatus.ACCEPTED)
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe OK
        headers(result).get("Refresh").value mustBe "10"
      }
    }

    "must redirect to submitted page when poll returns SUBMITTED" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(SubmissionStatus.SUBMITTED)
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerificationRequestSubmittedController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when poll returns FATAL_ERROR with error code 3000" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.FATAL_ERROR,
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "3000",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when poll returns FATAL_ERROR with error code 1000" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.FATAL_ERROR,
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "1000",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to VerifyDepartmentalErrorSubmitAgainController when poll returns FATAL_ERROR with error code 2005" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.FATAL_ERROR,
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "2005",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to SM-09 when poll returns DEPARTMENTAL_ERROR with error code 3000" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.DEPARTMENTAL_ERROR,
          govTalkErrorStatus = Some(
            DepartmentalError(
              errorCode = Some("3000"),
              errorText = "Departmental error"
            )
          )
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorSubmitAgainController
            .onPageLoad()
            .url
      }
    }

    "must redirect to departmental error page when poll returns DEPARTMENTAL_ERROR with error code 3001" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.DEPARTMENTAL_ERROR,
          govTalkErrorStatus = Some(
            DepartmentalError(
              errorCode = Some("3001"),
              errorText = "Departmental error"
            )
          )
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifyDepartmentalErrorController
            .onPageLoad()
            .url
      }
    }

    "must redirect to Verification Not Submitted Warning when poll returns FATAL_ERROR with another error code" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.FATAL_ERROR,
          govTalkErrorStatus = Some(
            FatalError(
              errorCode = "9999",
              errorText = "Fatal error"
            )
          )
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerificationNotSubmittedWarningController
            .onPageLoad()
            .url
      }
    }

    "must redirect to Verification Not Submitted Warning when poll returns FATAL_ERROR without GovTalk error details" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(
          status = SubmissionStatus.FATAL_ERROR,
          govTalkErrorStatus = None
        )
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerificationNotSubmittedWarningController
            .onPageLoad()
            .url
      }
    }

    "must redirect to send error page when poll returns SEND_ERROR" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(SubmissionStatus.SEND_ERROR)
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifySendErrorController
            .onPageLoad()
            .url
      }
    }

    "must redirect to verify-send-error when poll returns TIMED_OUT" in {
      val mockService = mock[VerificationService]

      mockPollResponse(
        mockService,
        pollResponse(SubmissionStatus.TIMED_OUT)
      )

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.verify.routes.VerifySendErrorController
            .onPageLoad()
            .url
      }
    }

    def pollResponseWithStatus(pollStatus: SubmissionStatus): ChrisPollResponse =
      ChrisPollResponse(
        status = pollStatus,
        correlationId = "corr-id",
        pollUrl = None,
        pollInterval = None,
        error = None,
        irMarkReceived = None,
        lastMessageDate = None,
        acceptedTime = None
      )

    def redirectForPollStatus(pollStatus: SubmissionStatus): String = {
      val mockService = mock[VerificationService]

      val ua =
        emptyUserAnswers
          .set(VerificationSubmissionDetailsPage, submissionDetails)
          .success
          .value

      when(
        mockService.isPollDue(any[VerificationSubmissionDetails], any[Int])
      ).thenReturn(true)
      when(
        mockService.pollStatusAndPersist(
          any[UserAnswers],
          any[VerificationSubmissionDetails]
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(pollResponseWithStatus(pollStatus)))

      val application =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(bind[VerificationService].toInstance(mockService))
          .build()

      running(application) {
        val result = route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value
      }
    }

    "must redirect to verify-send-error (SM-06) when poll returns SEND_ERROR" in {
      redirectForPollStatus(SubmissionStatus.SEND_ERROR) mustBe
        controllers.verify.routes.VerifySendErrorController.onPageLoad().url
    }

    "must redirect to verify-send-error (SM-06) when poll returns TIMED_OUT" in {
      redirectForPollStatus(SubmissionStatus.TIMED_OUT) mustBe
        controllers.verify.routes.VerifySendErrorController.onPageLoad().url
    }

    "must redirect to departmental-error (SM-07) when poll returns DEPARTMENTAL_ERROR" in {
      redirectForPollStatus(SubmissionStatus.DEPARTMENTAL_ERROR) mustBe
        controllers.verify.routes.VerifyDepartmentalErrorController.onPageLoad().url
    }

    "must redirect to verification-not-submitted warning (SM-08) when poll returns FATAL_ERROR" in {
      redirectForPollStatus(SubmissionStatus.FATAL_ERROR) mustBe
        controllers.verify.routes.VerificationNotSubmittedWarningController.onPageLoad().url
    }

    "must redirect to recovery when VerificationSubmissionDetailsPage is missing" in {
      val mockService = mock[VerificationService]

      val application =
        applicationWith(mockService)

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url

        verify(mockService, never()).pollStatusAndPersist(
          any[UserAnswers],
          any[VerificationSubmissionDetails]
        )(any[HeaderCarrier])
      }
    }

    "must redirect to recovery when polling service fails" in {
      val mockService = mock[VerificationService]

      when(
        mockService.isPollDue(any[VerificationSubmissionDetails], any[Int])
      ).thenReturn(true)
      when(
        mockService.pollStatusAndPersist(
          any[UserAnswers],
          any[VerificationSubmissionDetails]
        )(any[HeaderCarrier])
      ).thenReturn(Future.failed(new RuntimeException("boom")))

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.routes.JourneyRecoveryController
            .onPageLoad()
            .url
      }
    }

    "must show the sending page without polling when the first poll interval has not elapsed" in {
      val mockService = mock[VerificationService]

      when(
        mockService.isPollDue(any[VerificationSubmissionDetails], any[Int])
      ).thenReturn(false)

      val application =
        applicationWith(
          mockService,
          userAnswersWithSubmissionDetails
        )

      running(application) {
        val result =
          route(application, FakeRequest(GET, onPollRoute)).value

        status(result) mustBe OK
        header("Refresh", result).value mustBe "10"

        verify(mockService, never()).pollStatusAndPersist(
          any[UserAnswers],
          any[VerificationSubmissionDetails]
        )(any[HeaderCarrier])
      }
    }
  }
}
