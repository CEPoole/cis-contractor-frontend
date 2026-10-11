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

package services

import base.SpecBase
import connectors.ConstructionIndustrySchemeConnector
import generators.ModelGenerators
import models.*
import models.requests.*
import models.response.*
import models.verify.ContractorEmailConfirmationStored.DifferentEmail
import models.verify.*
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{verify, *}
import org.scalatest.RecoverMethods.recoverToExceptionIf
import org.scalatestplus.mockito.MockitoSugar
import pages.QuestionPage
import pages.verify.*
import play.api.i18n.Messages
import play.api.libs.json.{JsPath, Writes}
import play.api.mvc.AnyContent
import play.api.test.FakeRequest
import queries.CisIdQuery
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier

import play.api.libs.json.Json
import utils.SubmissionUtils

import java.time.{Clock, Instant, LocalDateTime, ZoneId}
import scala.concurrent.{ExecutionContext, Future}

final class VerificationServiceSpec extends SpecBase with MockitoSugar with ModelGenerators {

  implicit val hc: HeaderCarrier    = HeaderCarrier()
  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val msgs: Messages       = messages(app)

  private val instanceId = "INST-123"

  private val verifiedSubcontractor: Subcontractor =
    arbitrarySubcontractor.arbitrary.sample.value.copy(
      subcontractorId = 1L,
      verified = Some("Y")
    )

  private val unverifiedSub1: Subcontractor =
    arbitrarySubcontractor.arbitrary.sample.value.copy(
      subcontractorId = 2L,
      verified = Some("N")
    )

  private val unverifiedSub2: Subcontractor =
    arbitrarySubcontractor.arbitrary.sample.value.copy(
      subcontractorId = 3L,
      verified = None
    )

  private val responseWithSubcontractors =
    GetNewestVerificationBatchResponse(
      scheme = None,
      subcontractors = Seq(
        verifiedSubcontractor,
        unverifiedSub1,
        unverifiedSub2
      ),
      verificationBatch = None,
      verifications = Nil,
      submission = None,
      monthlyReturn = None,
      monthlyReturnSubmission = None
    )

  private val ukZone          = ZoneId.of("Europe/London")
  private val submissionUtils = new SubmissionUtils(applicationConfig)

  // submittedAt used by the poll tests is 2026-06-15T03:30:52 London time (BST, UTC+1);
  // the configured polling window is submission-poll-timeout-seconds = 60
  private val withinWindowClock: Clock    = Clock.fixed(Instant.parse("2026-06-15T02:31:00Z"), ukZone)
  private val exhaustedWindowClock: Clock = Clock.fixed(Instant.parse("2026-06-15T02:32:00Z"), ukZone)

  private def newestResponseWithBatchStatus(status: String): GetNewestVerificationBatchResponse =
    responseWithSubcontractors.copy(
      verificationBatch = Some(
        VerificationBatch(
          verificationBatchId = 1L,
          status = Some(status),
          verificationNumber = None
        )
      )
    )

  private def buildService(
    connector: ConstructionIndustrySchemeConnector,
    repo: SessionRepository,
    manageService: CisManageService = mock[CisManageService],
    requestBuilder: ChrisVerificationRequestBuilder = mock[ChrisVerificationRequestBuilder],
    clock: Clock = withinWindowClock
  ): VerificationService =
    new VerificationService(
      connector,
      manageService,
      requestBuilder,
      repo,
      submissionUtils,
      clock
    )

  private def currentSubcontractor(
    id: Long,
    ref: Option[Long]
  ): SubcontractorCurrentVerification =
    SubcontractorCurrentVerification(
      subcontractorId = id,
      subbieResourceRef = ref,
      firstName = None,
      secondName = None,
      surname = None,
      tradingName = None,
      utr = None,
      nino = None,
      crn = None,
      partnerUtr = None,
      partnershipTradingName = None,
      subcontractorType = None,
      addressLine1 = None,
      addressLine2 = None,
      addressLine3 = None,
      addressLine4 = None,
      country = None,
      postcode = None,
      emailAddress = None,
      phoneNumber = None,
      mobilePhoneNumber = None,
      worksReferenceNumber = None,
      matched = None,
      autoVerified = None,
      verified = None,
      verificationNumber = None,
      taxTreatment = None,
      verificationDate = None,
      version = None,
      updatedTaxTreatment = None,
      lastMonthlyReturnDate = None,
      pendingVerifications = None
    )

  "VerificationService.refreshNewestVerificationBatch" - {

    "must fetch newest verification batch, store response and unverified subcontractors, persist to session repo and return updated answers" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.refreshNewestVerificationBatch(userAnswers).futureValue

      result.get(NewestVerificationBatchResponsePage) mustBe Some(responseWithSubcontractors)

      result.get(UnverifiedSubcontractorsPage) mustBe
        Some(Seq(unverifiedSub1, unverifiedSub2))

      verify(mockConnector).getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }

    "must remove selected subcontractors that no longer exist in the newest subcontractor list" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val selectedSubcontractors =
        Set(
          SubcontractorViewModel("2", "Existing selected subcontractor"),
          SubcontractorViewModel("999", "Removed selected subcontractor")
        )

      val selectedSubcontractorsToReverify =
        Set(
          SelectedSubcontractors("1", "Existing selected subcontractor to reverify"),
          SelectedSubcontractors("998", "Removed selected subcontractor to reverify")
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(SelectSubcontractorPage, selectedSubcontractors)
          .success
          .value
          .set(SelectSubcontractorsToReverifyPage, selectedSubcontractorsToReverify)
          .success
          .value

      val newestResponse =
        responseWithSubcontractors.copy(verifications = Seq.empty)

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(newestResponse))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.refreshNewestVerificationBatch(userAnswers).futureValue

      result.get(SelectSubcontractorPage) mustBe Some(
        Set(SubcontractorViewModel("2", "Existing selected subcontractor"))
      )

      result.get(SelectSubcontractorsToReverifyPage) mustBe Some(
        Set(SelectedSubcontractors("1", "Existing selected subcontractor to reverify"))
      )

      verify(mockConnector).getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }

    "must fail when CisIdQuery is missing and not call connector or repository" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ex = service.refreshNewestVerificationBatch(emptyUserAnswers).failed.futureValue
      ex.getMessage must include("InstanceIdQuery not found in session data")

      verify(mockConnector, never()).getNewestVerificationBatch(any[String])(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
    }

    "must propagate connector failure and not persist session" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("Failure")))

      val ex = service.refreshNewestVerificationBatch(userAnswers).failed.futureValue
      ex.getMessage must include("Failure")

      verify(mockRepo, never()).set(any[UserAnswers])
    }

    "must propagate failure when setting NewestVerificationBatchResponsePage fails and not write to session repo" in {

      final case class BadType(value: String)
      case object BadSetPage extends QuestionPage[BadType] {
        override def path: JsPath     = JsPath \ "badSetPage"
        override def toString: String = "badSetPage"
      }

      given Writes[BadType] = Writes[BadType] { _ =>
        throw new RuntimeException("writes-failed")
      }

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]

      class TestService(
        conn: ConstructionIndustrySchemeConnector,
        repo: SessionRepository,
        manageService: CisManageService,
        requestBuilder: ChrisVerificationRequestBuilder
      )(implicit ec: ExecutionContext)
          extends VerificationService(conn, manageService, requestBuilder, repo, submissionUtils, withinWindowClock) {

        def refreshAndForceBadSet(ua: UserAnswers)(implicit hc: HeaderCarrier): Future[UserAnswers] =
          for {
            instanceId <- ua.get(CisIdQuery)
                            .map(Future.successful)
                            .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found")))
            resp       <- conn.getNewestVerificationBatch(instanceId)
            _          <- Future.fromTry(ua.set(NewestVerificationBatchResponsePage, resp))
            bad        <- Future.fromTry(ua.set(BadSetPage, BadType("x")))
            _          <- repo.set(bad)
          } yield bad
      }

      val service =
        new TestService(mockConnector, mockRepo, mock[CisManageService], mock[ChrisVerificationRequestBuilder])

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors))

      val ex =
        recoverToExceptionIf[RuntimeException] {
          service.refreshAndForceBadSet(ua)
        }.futureValue

      ex.getMessage must include("writes-failed")
    }
  }

  "VerificationService.refreshSubmittedVerificationRequest" - {

    def responseWithSubmissionStatus(status: Option[String]): GetNewestVerificationBatchResponse =
      responseWithSubcontractors.copy(
        submission = Some(
          Submission(
            submissionId = 1L,
            activeObjectId = Some(1L),
            submissionRequestDate = Some(LocalDateTime.parse("2026-06-15T03:30:52")),
            status = status
          )
        )
      )

    "must return updated answers when the latest submission status is SUBMITTED" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      val response = responseWithSubmissionStatus(Some("SUBMITTED"))

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.refreshSubmittedVerificationRequest(userAnswers).futureValue

      result.get(NewestVerificationBatchResponsePage) mustBe Some(response)
    }

    "must return updated answers when the latest submission status is SUBMITTED_NO_RECEIPT" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      val response = responseWithSubmissionStatus(Some("SUBMITTED_NO_RECEIPT"))

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.refreshSubmittedVerificationRequest(userAnswers).futureValue

      result.get(NewestVerificationBatchResponsePage) mustBe Some(response)
    }

    "must fail when the latest submission status is not submitted" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubmissionStatus(Some("PENDING"))))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val ex = service.refreshSubmittedVerificationRequest(userAnswers).failed.futureValue

      ex.getMessage must include("Submitted verification request page cannot be accessed")
    }

    "must fail when the latest response has no submission" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors.copy(submission = None)))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val ex = service.refreshSubmittedVerificationRequest(userAnswers).failed.futureValue

      ex.getMessage mustBe
        "Submitted verification request page cannot be accessed for submission status: missing"
    }

    "must fail when the latest submission has no status" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubmissionStatus(None)))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val ex = service.refreshSubmittedVerificationRequest(userAnswers).failed.futureValue

      ex.getMessage mustBe
        "Submitted verification request page cannot be accessed for submission status: missing"
    }
  }

  "VerificationService.getCurrentVerificationBatch" - {

    val instanceId = "INST-123"

    val response =
      GetCurrentVerificationBatchResponse(
        subcontractors = Nil,
        verificationBatch = None,
        verifications = Nil
      )

    "must fetch current verification batch, store in UserAnswers, persist to session repo, and return updated answers" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.getCurrentVerificationBatch(ua).futureValue

      result.get(CurrentVerificationBatchResponsePage) mustBe Some(response)

      verify(mockConnector).getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])

      verify(mockRepo).set(
        org.mockito.ArgumentMatchers.argThat { (saved: UserAnswers) =>
          saved.get(CurrentVerificationBatchResponsePage).contains(response)
        }
      )

      verifyNoMoreInteractions(mockConnector)
    }

    "must fail when CisIdQuery (instance id) is missing and not call connector nor repo" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ex = service.getCurrentVerificationBatch(emptyUserAnswers).failed.futureValue
      ex.getMessage must include("InstanceIdQuery not found in session data")

      verify(mockConnector, never()).getCurrentVerificationBatch(any[String])(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }

    "must propagate connector failure and not write to session repo" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      val ex = service.getCurrentVerificationBatch(ua).failed.futureValue
      ex.getMessage must include("boom")

      verify(mockConnector).getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }

    "must propagate failure when setting CurrentVerificationBatchResponsePage fails and not write to session repo" in {

      final case class BadType(value: String)
      case object BadSetPage extends QuestionPage[BadType] {
        override def path: JsPath = JsPath \ "badSetPage"

        override def toString: String = "badSetPage"
      }

      given Writes[BadType] = Writes[BadType] { _ =>
        throw new RuntimeException("writes-failed")
      }

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]

      class TestService(
        conn: ConstructionIndustrySchemeConnector,
        repo: SessionRepository,
        manageService: CisManageService,
        requestBuilder: ChrisVerificationRequestBuilder
      )(implicit ec: ExecutionContext)
          extends VerificationService(conn, manageService, requestBuilder, repo, submissionUtils, withinWindowClock) {

        def refreshAndForceBadSet(ua: UserAnswers)(implicit hc: HeaderCarrier): Future[UserAnswers] =
          for {
            instanceId <- ua.get(CisIdQuery)
                            .map(Future.successful)
                            .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found in session data")))
            resp       <- conn.getCurrentVerificationBatch(instanceId)

            _ <- Future.fromTry(ua.set(CurrentVerificationBatchResponsePage, resp))

            bad <- Future.fromTry(ua.set(BadSetPage, BadType("x")))

            _ <- repo.set(bad)
          } yield bad
      }

      val service =
        new TestService(mockConnector, mockRepo, mock[CisManageService], mock[ChrisVerificationRequestBuilder])

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))

      val ex = service.refreshAndForceBadSet(ua).failed.futureValue
      ex.getMessage must include("writes-failed")

      verify(mockConnector).getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }
  }

  "VerificationService.getLastSubmittedVerificationBatch" - {

    val instanceId = "INST-123"

    val response =
      GetLastSubmittedVerificationBatchResponse(
        scheme = None,
        subcontractors = Nil,
        verifications = Nil,
        verificationBatch = None,
        submission = None
      )

    "must fetch last submitted verification batch, store in UserAnswers, persist to session repo, and return updated answers" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getLastSubmittedVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(response))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.getLastSubmittedVerificationBatch(ua).futureValue

      result.get(LastSubmittedVerificationBatchResponsePage) mustBe Some(response)

      verify(mockConnector).getLastSubmittedVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])

      verify(mockRepo).set(
        org.mockito.ArgumentMatchers.argThat { (saved: UserAnswers) =>
          saved.get(LastSubmittedVerificationBatchResponsePage).contains(response)
        }
      )

      verifyNoMoreInteractions(mockConnector)
    }

    "must fail when CisIdQuery (instance id) is missing and not call connector nor repo" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ex = service.getLastSubmittedVerificationBatch(emptyUserAnswers).failed.futureValue
      ex.getMessage must include("InstanceIdQuery not found in session data")

      verify(mockConnector, never()).getLastSubmittedVerificationBatch(any[String])(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }

    "must propagate connector failure and not write to session repo" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockConnector.getLastSubmittedVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      val ex = service.getLastSubmittedVerificationBatch(ua).failed.futureValue
      ex.getMessage must include("boom")

      verify(mockConnector).getLastSubmittedVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
      verifyNoMoreInteractions(mockConnector)
    }
  }

  "VerificationService.createVerificationBatchAndVerifications" - {

    val currentBatchResponse =
      GetCurrentVerificationBatchResponse(
        subcontractors = Seq(
          currentSubcontractor(10L, Some(1111L)),
          currentSubcontractor(20L, Some(2222L))
        ),
        verificationBatch = Some(
          VerificationBatchCurrentVerification(
            verificationBatchId = 999L,
            verifBatchResourceRef = Some(7777L)
          )
        ),
        verifications = Seq(
          VerificationCurrentVerification(
            verificationId = 1L,
            verificationBatchId = Some(999L),
            subcontractorId = Some(10L),
            verificationResourceRef = Some(1111L),
            subcontractorName = None,
            verificationNumber = None,
            taxTreatment = None,
            actionIndicator = None,
            proceed = None,
            matched = None
          )
        )
      )

    "must map selected subcontractor ids to resource refs, call connector, refresh current+newest and persist" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(CurrentVerificationBatchResponsePage, currentBatchResponse)
          .success
          .value

      val createResp = CreateVerificationBatchAndVerificationsResponse(verificationBatchResourceReference = 12345L)

      when(
        mockConnector.createVerificationBatchAndVerifications(any[CreateVerificationBatchAndVerificationsRequest])(
          any[HeaderCarrier]
        )
      )
        .thenReturn(Future.successful(createResp))

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(currentBatchResponse))

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors))

      when(mockRepo.set(any[UserAnswers])).thenReturn(Future.successful(true))

      val selectedIds = Seq(10L, 20L, 10L)

      val result = service
        .createVerificationBatchAndVerifications(
          userAnswers = ua,
          selectedSubcontractorIds = selectedIds,
          actionIndicator = Some("A")
        )
        .futureValue

      result.get(NewestVerificationBatchResponsePage) mustBe Some(responseWithSubcontractors)
      result.get(UnverifiedSubcontractorsPage) mustBe Some(Seq(unverifiedSub1, unverifiedSub2))

      val captor: ArgumentCaptor[CreateVerificationBatchAndVerificationsRequest] =
        ArgumentCaptor.forClass(classOf[CreateVerificationBatchAndVerificationsRequest])

      verify(mockConnector).createVerificationBatchAndVerifications(captor.capture())(any[HeaderCarrier])

      captor.getValue mustBe CreateVerificationBatchAndVerificationsRequest(
        instanceId = instanceId,
        verificationResourceReferences = Seq(1111L, 2222L),
        actionIndicator = Some("A")
      )

      verify(mockConnector).getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockConnector).getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])

      verify(mockRepo, org.mockito.Mockito.times(3)).set(any[UserAnswers])
    }

    "must map selected subcontractor ids to resource refs from the newest batch when current batch data is absent" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val newestResponse =
        responseWithSubcontractors.copy(
          subcontractors = Seq(
            unverifiedSub1.copy(subcontractorId = 2L, subbieResourceRef = Some(2222L))
          )
        )

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(NewestVerificationBatchResponsePage, newestResponse)
          .success
          .value

      val createResp = CreateVerificationBatchAndVerificationsResponse(verificationBatchResourceReference = 12345L)

      when(
        mockConnector.createVerificationBatchAndVerifications(any[CreateVerificationBatchAndVerificationsRequest])(
          any[HeaderCarrier]
        )
      )
        .thenReturn(Future.successful(createResp))

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(currentBatchResponse))

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors))

      when(mockRepo.set(any[UserAnswers])).thenReturn(Future.successful(true))

      service
        .createVerificationBatchAndVerifications(
          userAnswers = ua,
          selectedSubcontractorIds = Seq(2L),
          actionIndicator = None
        )
        .futureValue

      val captor: ArgumentCaptor[CreateVerificationBatchAndVerificationsRequest] =
        ArgumentCaptor.forClass(classOf[CreateVerificationBatchAndVerificationsRequest])

      verify(mockConnector).createVerificationBatchAndVerifications(captor.capture())(any[HeaderCarrier])

      captor.getValue.verificationResourceReferences mustBe Seq(2222L)
    }

    "must fail when no subcontractors selected" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(CurrentVerificationBatchResponsePage, currentBatchResponse)
          .success
          .value

      val ex = service.createVerificationBatchAndVerifications(ua, Seq.empty).failed.futureValue
      ex.getMessage must include("No subcontractors selected")

      verify(mockConnector, never()).createVerificationBatchAndVerifications(any())(any())
      verify(mockRepo, never()).set(any())
    }

    "must fail when no verification batch data is available" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      val ex =
        service
          .createVerificationBatchAndVerifications(ua, Seq(10L))
          .failed
          .futureValue

      ex.getMessage must include(
        "Neither CurrentVerificationBatchResponsePage nor NewestVerificationBatchResponsePage found in session data"
      )

      verify(mockConnector, never()).createVerificationBatchAndVerifications(any())(any())
      verify(mockRepo, never()).set(any())
    }

    "must fail when selected subcontractor id has no subbieResourceRef in current batch" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(CurrentVerificationBatchResponsePage, currentBatchResponse)
          .success
          .value

      val ex =
        service
          .createVerificationBatchAndVerifications(ua, Seq(30L))
          .failed
          .futureValue

      ex.getMessage must include("Missing subbieResourceRef for subcontractorId=30")

      verify(mockConnector, never()).createVerificationBatchAndVerifications(any())(any())
      verify(mockRepo, never()).set(any())
    }
  }

  "VerificationService.modifyVerificationBatchAndVerifications" - {

    "must call modify endpoint, refresh current+newest and persist" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val currentResp =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil
        )

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(CurrentVerificationBatchResponsePage, currentResp)
          .success
          .value

      val req = ModifyVerificationsRequest(
        instanceId = instanceId,
        deleteVerifications = None,
        createVerifications = None
      )

      when(mockConnector.modifyVerificationBatch(eqTo(req))(any[HeaderCarrier]))
        .thenReturn(Future.successful(()))

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(currentResp))

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result =
        service
          .modifyVerificationBatchAndVerifications(ua, req)
          .futureValue

      result.get(NewestVerificationBatchResponsePage) mustBe Some(responseWithSubcontractors)
      result.get(UnverifiedSubcontractorsPage) mustBe Some(Seq(unverifiedSub1, unverifiedSub2))

      verify(mockConnector).modifyVerificationBatch(eqTo(req))(any[HeaderCarrier])
      verify(mockConnector).getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockConnector).getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo, org.mockito.Mockito.times(3)).set(any[UserAnswers])
    }

    "must propagate failure when modify endpoint fails and not refresh/persist" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      val req = ModifyVerificationsRequest(
        instanceId = instanceId,
        deleteVerifications = None,
        createVerifications = None
      )

      when(mockConnector.modifyVerificationBatch(eqTo(req))(any[HeaderCarrier]))
        .thenReturn(Future.failed(new RuntimeException("boom")))

      val ex =
        service
          .modifyVerificationBatchAndVerifications(ua, req)
          .failed
          .futureValue

      ex.getMessage must include("boom")

      verify(mockConnector).modifyVerificationBatch(eqTo(req))(any[HeaderCarrier])
      verify(mockConnector, never()).getCurrentVerificationBatch(any[String])(any[HeaderCarrier])
      verify(mockConnector, never()).getNewestVerificationBatch(any[String])(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
    }
  }

  "VerificationService.deleteVerification" - {

    "must call delete endpoint, refresh current+newest and persist" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val currentResp =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil
        )

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      val request =
        DeleteVerificationRequest(
          instanceId = instanceId,
          verificationResourceRef = 1111L
        )

      val deleteResponse =
        DeleteVerificationResponse(
          verificationsCounter = Some(1L)
        )

      when(mockConnector.deleteVerification(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(deleteResponse))

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(currentResp))

      when(mockConnector.getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(responseWithSubcontractors))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result =
        service
          .deleteVerification(ua, 1111L)
          .futureValue

      result mustBe deleteResponse

      verify(mockConnector).deleteVerification(eqTo(request))(any[HeaderCarrier])
      verify(mockConnector).getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockConnector).getNewestVerificationBatch(eqTo(instanceId))(any[HeaderCarrier])
      verify(mockRepo, times(3)).set(any[UserAnswers])
    }

    "must fail when instance id is missing" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val ex =
        service
          .deleteVerification(emptyUserAnswers, 1111L)
          .failed
          .futureValue

      ex.getMessage mustBe "InstanceIdQuery not found in session data"

      verify(mockConnector, never()).deleteVerification(any[DeleteVerificationRequest])(any[HeaderCarrier])
      verify(mockRepo, never()).set(any[UserAnswers])
    }
  }

  "VerificationService.createSubmitAndPersistVerificationSubmission" - {

    "must create, submit and save details" in {
      val mockConnector      = mock[ConstructionIndustrySchemeConnector]
      val mockRepo           = mock[SessionRepository]
      val mockRequestBuilder = mock[ChrisVerificationRequestBuilder]
      val service            = buildService(mockConnector, mockRepo, requestBuilder = mockRequestBuilder)

      val currentBatch =
        GetCurrentVerificationBatchResponse(
          subcontractors = Seq.empty,
          verificationBatch = Some(
            VerificationBatchCurrentVerification(
              verificationBatchId = 999L,
              verifBatchResourceRef = Some(7777L)
            )
          ),
          verifications = Seq(
            VerificationCurrentVerification(
              verificationId = 1L,
              verificationBatchId = Some(999L),
              subcontractorId = Some(10L),
              verificationResourceRef = Some(1111L),
              subcontractorName = None,
              verificationNumber = None,
              taxTreatment = None,
              actionIndicator = None,
              proceed = None,
              matched = None
            )
          )
        )

      val ua =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(CurrentVerificationBatchResponsePage, currentBatch)
          .success
          .value
          .set(ContractorEmailConfirmationStoredPage, DifferentEmail)
          .success
          .value
          .set(EmailAddressPage, "test@test.com")
          .success
          .value

      implicit val request: DataRequest[AnyContent] =
        DataRequest(
          request = FakeRequest(),
          userId = "user-id",
          userAnswers = ua,
          employerReference = Some(EmployerReference("123", "AB456"))
        )

      val chrisRequest =
        ChrisVerificationRequest(
          instanceId = instanceId,
          isAgent = false,
          clientTaxOfficeNumber = "123",
          clientTaxOfficeRef = "AB456",
          contractorUTR = "1234567890",
          contractorAORef = "AO123",
          verificationBatchId = "999",
          verificationBatchResourceRef = "7777",
          emailRecipient = Some("test@test.com"),
          subcontractors = Seq.empty,
          verifications = Seq.empty
        )

      val chrisResponse =
        ChrisSubmissionResponse(
          submissionId = "13602",
          status = "ACCEPTED",
          hmrcMarkGenerated = "hmrc-mark",
          correlationId = Some("corr-id"),
          responseEndPoint = Some(ResponseEndPointDto("http://localhost/poll", 5)),
          gatewayTimestamp = Some("2026-06-15T03:30:52")
        )

      when(mockConnector.getCurrentVerificationBatch(eqTo(instanceId))(any[HeaderCarrier]))
        .thenReturn(Future.successful(currentBatch))

      when(
        mockConnector.createSubmissionForVerification(any[CreateSubmissionForVerificationRequest])(any[HeaderCarrier])
      ).thenReturn(Future.successful(CreateSubmissionForVerificationResponse(13602L)))

      when(
        mockRequestBuilder.build(any[UserAnswers], eqTo(false), eqTo(EmployerReference("123", "AB456")))(
          any[HeaderCarrier],
          any[Messages]
        )
      ).thenReturn(Future.successful(chrisRequest))

      when(mockConnector.submitVerificationToChris(eqTo(13602L), eqTo(chrisRequest))(any[HeaderCarrier]))
        .thenReturn(Future.successful(chrisResponse))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result =
        service.createSubmitAndPersistVerificationSubmission.futureValue

      result mustBe chrisResponse

      verify(mockConnector).createSubmissionForVerification(any[CreateSubmissionForVerificationRequest])(
        any[HeaderCarrier]
      )
      verify(mockConnector).submitVerificationToChris(eqTo(13602L), eqTo(chrisRequest))(any[HeaderCarrier])
      verify(mockRepo, times(2)).set(any[UserAnswers])
    }
  }

  "VerificationService.pollStatusAndPersist" - {

    "must poll ChRIS and return response" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val details =
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

      val pollResponse =
        ChrisPollResponse(
          status = SubmissionStatus.SUBMITTED,
          correlationId = "new-corr",
          pollUrl = None,
          pollInterval = None,
          error = None,
          irMarkReceived = Some("ggis-mark"),
          lastMessageDate = Some("2026-06-15T03:30:54"),
          acceptedTime = Some("2026-06-15T03:30:55")
        )

      when(mockConnector.getSubmissionStatus(eqTo("http://localhost/poll"), eqTo("13602"))(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollResponse))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result =
        service.pollStatusAndPersist(emptyUserAnswers, details).futureValue

      result mustBe pollResponse

      verify(mockConnector).getSubmissionStatus(eqTo("http://localhost/poll"), eqTo("13602"))(any[HeaderCarrier])
    }

    "must fail when pollUrl is missing" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val details =
        VerificationSubmissionDetails(
          submissionId = "13602",
          status = "ACCEPTED",
          hmrcMarkGenerated = "hmrc-mark",
          hmrcMarkGgis = None,
          correlationId = Some("corr-id"),
          pollUrl = None,
          pollIntervalSeconds = Some(5),
          submittedAt = LocalDateTime.parse("2026-06-15T03:30:52"),
          lastMessageDate = None,
          timedOut = false
        )

      val ex =
        service.pollStatusAndPersist(emptyUserAnswers, details).failed.futureValue

      ex.getMessage mustBe "Poll URL missing in submission details"

      verify(mockConnector, never()).getSubmissionStatus(any[String], any[String])(any[HeaderCarrier])
    }
  }

  "VerificationService.pollStatusAndPersist polling window" - {

    def acceptedDetails: VerificationSubmissionDetails =
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

    def acceptedPollResponse: ChrisPollResponse =
      ChrisPollResponse(
        status = SubmissionStatus.ACCEPTED,
        correlationId = "corr-id",
        pollUrl = Some("http://localhost/poll"),
        pollInterval = Some(5),
        error = None,
        irMarkReceived = None,
        lastMessageDate = None,
        acceptedTime = None
      )

    def poll(
      pollResponse: ChrisPollResponse,
      clock: Clock
    ): (ChrisPollResponse, VerificationSubmissionDetails) = {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo, clock = clock)

      when(mockConnector.getSubmissionStatus(any[String], any[String])(any[HeaderCarrier]))
        .thenReturn(Future.successful(pollResponse))
      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val result = service.pollStatusAndPersist(emptyUserAnswers, acceptedDetails).futureValue

      val uaCaptor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
      verify(mockRepo).set(uaCaptor.capture())
      val persisted                             = uaCaptor.getValue.get(VerificationSubmissionDetailsPage).value

      (result, persisted)
    }

    "must pass ACCEPTED through unchanged while the polling window is open" in {
      val (result, persisted) = poll(acceptedPollResponse, withinWindowClock)

      result.status mustBe SubmissionStatus.ACCEPTED
      persisted.status mustBe "ACCEPTED"
      persisted.timedOut mustBe false
    }

    "must return SEND_ERROR when the window is exhausted and the poll carried a timeOut error" in {
      val response = acceptedPollResponse.copy(
        error = Some(Json.obj("number" -> "500", "type" -> "timeOut", "text" -> "timeOut")),
        govTalkErrorStatus = Some(GovTalkErrorStatus.ServerError(500))
      )

      val (result, persisted) = poll(response, exhaustedWindowClock)

      result.status mustBe SubmissionStatus.SEND_ERROR
      persisted.status mustBe "SEND_ERROR"
      persisted.timedOut mustBe true
    }

    "must return SEND_ERROR when the window is exhausted and the poll got no response (connection refused)" in {
      val response = acceptedPollResponse.copy(govTalkErrorStatus = Some(GovTalkErrorStatus.NoResponse))

      val (result, persisted) = poll(response, exhaustedWindowClock)

      result.status mustBe SubmissionStatus.SEND_ERROR
      persisted.timedOut mustBe true
    }

    "must return TIMED_OUT when the window is exhausted but the poll was a clean still-pending response" in {
      val (result, persisted) = poll(acceptedPollResponse, exhaustedWindowClock)

      result.status mustBe SubmissionStatus.TIMED_OUT
      persisted.status mustBe "TIMED_OUT"
      persisted.timedOut mustBe true
    }

    "must pass a terminal status through unchanged even when the window is exhausted" in {
      val response = acceptedPollResponse.copy(status = SubmissionStatus.SUBMITTED)

      val (result, persisted) = poll(response, exhaustedWindowClock)

      result.status mustBe SubmissionStatus.SUBMITTED
      persisted.status mustBe "SUBMITTED"
      persisted.timedOut mustBe false
    }

    "must pass DEPARTMENTAL_ERROR through unchanged when the window is exhausted" in {
      val response = acceptedPollResponse.copy(status = SubmissionStatus.DEPARTMENTAL_ERROR)

      val (result, _) = poll(response, exhaustedWindowClock)

      result.status mustBe SubmissionStatus.DEPARTMENTAL_ERROR
    }
  }

  "VerificationService.isPollDue" - {

    def detailsWith(
      submittedAt: LocalDateTime,
      lastMessageDate: Option[LocalDateTime]
    ): VerificationSubmissionDetails =
      VerificationSubmissionDetails(
        submissionId = "13602",
        status = "ACCEPTED",
        hmrcMarkGenerated = "hmrc-mark",
        hmrcMarkGgis = None,
        correlationId = Some("corr-id"),
        pollUrl = Some("http://localhost/poll"),
        pollIntervalSeconds = Some(5),
        submittedAt = submittedAt,
        lastMessageDate = lastMessageDate,
        timedOut = false
      )

    // withinWindowClock = 2026-06-15T02:31:00Z = 2026-06-15T03:31:00 London (BST)
    def service: VerificationService =
      buildService(
        mock[ConstructionIndustrySchemeConnector],
        mock[SessionRepository],
        clock = withinWindowClock
      )

    "must return false for the first poll before the poll interval has elapsed since submission" in {
      // submittedAt + 5s = 03:31:05, which is after now (03:31:00)
      service.isPollDue(detailsWith(LocalDateTime.parse("2026-06-15T03:31:00"), None), 5) mustBe false
    }

    "must return true for the first poll once the poll interval has elapsed since submission" in {
      // submittedAt + 5s = 03:30:57, which is before now (03:31:00)
      service.isPollDue(detailsWith(LocalDateTime.parse("2026-06-15T03:30:52"), None), 5) mustBe true
    }

    "must return false after a poll when the interval has not elapsed since the last message" in {
      // lastMessageDate + 5s = 03:31:04, which is after now (03:31:00)
      service.isPollDue(
        detailsWith(
          LocalDateTime.parse("2026-06-15T03:30:52"),
          Some(LocalDateTime.parse("2026-06-15T03:30:59"))
        ),
        5
      ) mustBe false
    }

    "must return true after a poll once the interval has elapsed since the last message" in {
      // lastMessageDate + 5s = 03:30:57, which is before now (03:31:00)
      service.isPollDue(
        detailsWith(
          LocalDateTime.parse("2026-06-15T03:31:00"),
          Some(LocalDateTime.parse("2026-06-15T03:30:52"))
        ),
        5
      ) mustBe true
    }
  }

  "VerificationService.anyUnmatchedResourceRefsStillPresent" - {

    "must return true when an unmatched verificationResourceRef is still on the live list" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val lastSubmitted =
        GetLastSubmittedVerificationBatchResponse(
          scheme = None,
          subcontractors = Nil,
          verifications = Seq(
            VerificationLastVerification(
              verificationId = 1001L,
              verificationBatchId = Some(99L),
              verificationResourceRef = Some(10L),
              matched = None,
              verificationNumber = None,
              taxTreatment = Some("unmatched"),
              subcontractorName = Some("John Smith"),
              subcontractorId = Some(22L),
              actionIndicator = Some("verify")
            )
          ),
          verificationBatch = None,
          submission = None
        )

      when(mockConnector.getSubcontractorList(eqTo("900063"))(any[HeaderCarrier]))
        .thenReturn(
          Future.successful(
            GetSubcontractorListResponse(Seq(SubcontractorListItem(22L, Some(10L))))
          )
        )

      service.anyUnmatchedResourceRefsStillPresent("900063", lastSubmitted).futureValue mustBe true
      verify(mockConnector).getSubcontractorList(eqTo("900063"))(any[HeaderCarrier])
    }

    "must return false when unmatched verificationResourceRefs are not on the live list" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val lastSubmitted =
        GetLastSubmittedVerificationBatchResponse(
          scheme = None,
          subcontractors = Nil,
          verifications = Seq(
            VerificationLastVerification(
              verificationId = 1001L,
              verificationBatchId = Some(99L),
              verificationResourceRef = Some(10L),
              matched = None,
              verificationNumber = None,
              taxTreatment = Some("unmatched"),
              subcontractorName = Some("John Smith"),
              subcontractorId = Some(22L),
              actionIndicator = Some("verify")
            )
          ),
          verificationBatch = None,
          submission = None
        )

      when(mockConnector.getSubcontractorList(eqTo("900063"))(any[HeaderCarrier]))
        .thenReturn(
          Future.successful(
            GetSubcontractorListResponse(Seq(SubcontractorListItem(11L, Some(999L))))
          )
        )

      service.anyUnmatchedResourceRefsStillPresent("900063", lastSubmitted).futureValue mustBe false
    }

    "must return false without calling the connector when there are no recreatable unmatched subcontractors" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val lastSubmitted =
        GetLastSubmittedVerificationBatchResponse(
          scheme = None,
          subcontractors = Nil,
          verifications = Seq(
            VerificationLastVerification(
              verificationId = 1001L,
              verificationBatchId = Some(99L),
              verificationResourceRef = Some(10L),
              matched = None,
              verificationNumber = None,
              taxTreatment = Some("unmatched"),
              subcontractorName = Some("John Smith"),
              subcontractorId = None,
              actionIndicator = Some("verify")
            )
          ),
          verificationBatch = None,
          submission = None
        )

      service
        .anyUnmatchedResourceRefsStillPresent("900063", lastSubmitted)
        .futureValue mustBe false

      verify(mockConnector, never())
        .getSubcontractorList(any[String])(any[HeaderCarrier])
    }
  }

  "VerificationService.recreateCurrentBatchFromUnmatchedVerifications" - {

    val unmatchedSubcontractor =
      SubcontractorLastVerification(
        subcontractorId = 22L,
        subbieResourceRef = Some(222L),
        subcontractorType = None,
        utr = None
      )

    val matchedSubcontractor =
      SubcontractorLastVerification(
        subcontractorId = 33L,
        subbieResourceRef = Some(333L),
        subcontractorType = None,
        utr = None
      )

    val unmatchedVerification =
      VerificationLastVerification(
        verificationId = 1L,
        verificationBatchId = Some(1L),
        verificationResourceRef = Some(100L),
        matched = Some("N"),
        verificationNumber = None,
        taxTreatment = None,
        subcontractorName = None,
        subcontractorId = Some(22L),
        actionIndicator = Some("match")
      )

    val matchedVerification =
      VerificationLastVerification(
        verificationId = 2L,
        verificationBatchId = Some(1L),
        verificationResourceRef = Some(101L),
        matched = Some("Y"),
        verificationNumber = Some("V0000000002"),
        taxTreatment = Some("net"),
        subcontractorName = None,
        subcontractorId = Some(33L),
        actionIndicator = Some("verify")
      )

    val lastSubmittedResponse =
      GetLastSubmittedVerificationBatchResponse(
        scheme = None,
        subcontractors = Seq(
          unmatchedSubcontractor,
          matchedSubcontractor
        ),
        verifications = Seq(
          unmatchedVerification,
          matchedVerification
        ),
        verificationBatch = None,
        submission = None
      )

    "must create a new verification batch using only unmatched subcontractors when no current batch exists" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val currentResponse =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(
            LastSubmittedVerificationBatchResponsePage,
            lastSubmittedResponse
          )
          .success
          .value

      when(
        mockConnector.getCurrentVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      )
        .thenReturn(Future.successful(currentResponse))
        .thenReturn(Future.successful(currentResponse))

      when(
        mockConnector.createVerificationBatchAndVerifications(
          any[CreateVerificationBatchAndVerificationsRequest]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          CreateVerificationBatchAndVerificationsResponse(
            verificationBatchResourceReference = 12345L
          )
        )
      )

      when(
        mockConnector.getNewestVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(newestResponseWithBatchStatus("SUBMITTED")))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val requestCaptor =
        ArgumentCaptor.forClass(
          classOf[CreateVerificationBatchAndVerificationsRequest]
        )

      service
        .recreateCurrentBatchFromUnmatchedVerifications(instanceId, userAnswers)
        .futureValue

      verify(mockConnector)
        .createVerificationBatchAndVerifications(
          requestCaptor.capture()
        )(any[HeaderCarrier])

      requestCaptor.getValue mustBe
        CreateVerificationBatchAndVerificationsRequest(
          instanceId = instanceId,
          verificationResourceReferences = Seq(222L),
          actionIndicator = None
        )

      verify(mockConnector)
        .getCurrentVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])

      verify(mockConnector, times(2))
        .getNewestVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])

      verify(mockConnector, never())
        .modifyVerificationBatch(
          any[ModifyVerificationsRequest]
        )(any[HeaderCarrier])
    }

    "must create a new verification batch when the current batch has no verifBatchResourceRef" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val currentResponse =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = Some(
            VerificationBatchCurrentVerification(
              verificationBatchId = 1L,
              verifBatchResourceRef = None
            )
          ),
          verifications = Nil
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(
            LastSubmittedVerificationBatchResponsePage,
            lastSubmittedResponse
          )
          .success
          .value

      val createResponse =
        CreateVerificationBatchAndVerificationsResponse(
          verificationBatchResourceReference = 12345L
        )

      when(
        mockConnector.getCurrentVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      )
        .thenReturn(Future.successful(currentResponse))
        .thenReturn(Future.successful(currentResponse))

      when(
        mockConnector.createVerificationBatchAndVerifications(
          any[CreateVerificationBatchAndVerificationsRequest]
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(createResponse))

      when(
        mockConnector.getNewestVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(newestResponseWithBatchStatus("SUBMITTED")))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val requestCaptor =
        ArgumentCaptor.forClass(
          classOf[CreateVerificationBatchAndVerificationsRequest]
        )

      service
        .recreateCurrentBatchFromUnmatchedVerifications(
          instanceId,
          userAnswers
        )
        .futureValue

      verify(mockConnector)
        .createVerificationBatchAndVerifications(
          requestCaptor.capture()
        )(any[HeaderCarrier])

      requestCaptor.getValue mustBe
        CreateVerificationBatchAndVerificationsRequest(
          instanceId = instanceId,
          verificationResourceReferences = Seq(222L),
          actionIndicator = None
        )

      verify(mockConnector, never())
        .modifyVerificationBatch(
          any[ModifyVerificationsRequest]
        )(any[HeaderCarrier])
    }

    "must fail before refreshing the current batch when an unmatched verification has no subcontractorId" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val responseWithoutSubcontractorId =
        lastSubmittedResponse.copy(
          subcontractors = Seq(unmatchedSubcontractor),
          verifications = Seq(
            unmatchedVerification.copy(
              subcontractorId = None
            )
          )
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(
            LastSubmittedVerificationBatchResponsePage,
            responseWithoutSubcontractorId
          )
          .success
          .value

      val exception =
        service
          .recreateCurrentBatchFromUnmatchedVerifications(
            instanceId,
            userAnswers
          )
          .failed
          .futureValue

      exception.getMessage mustBe
        "No unmatched subcontractor references found in LastSubmittedVerificationBatchResponsePage"

      verify(mockConnector, never())
        .getCurrentVerificationBatch(
          any[String]
        )(any[HeaderCarrier])

      verify(mockConnector, never())
        .createVerificationBatchAndVerifications(
          any[CreateVerificationBatchAndVerificationsRequest]
        )(any[HeaderCarrier])

      verify(mockConnector, never())
        .modifyVerificationBatch(
          any[ModifyVerificationsRequest]
        )(any[HeaderCarrier])

      verify(mockRepo, never())
        .set(any[UserAnswers])
    }

    "must return existing current verification batch without recreating it" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val currentResponse =
        GetCurrentVerificationBatchResponse(
          subcontractors = Seq(
            currentSubcontractor(
              id = 22L,
              ref = Some(222L)
            )
          ),
          verificationBatch = Some(
            VerificationBatchCurrentVerification(
              verificationBatchId = 1L,
              verifBatchResourceRef = Some(12345L)
            )
          ),
          verifications = Seq(
            VerificationCurrentVerification(
              verificationId = 1L,
              verificationBatchId = Some(1L),
              subcontractorId = Some(22L),
              verificationResourceRef = Some(999L),
              subcontractorName = Some("Existing Subcontractor"),
              verificationNumber = None,
              taxTreatment = None,
              actionIndicator = None,
              proceed = None,
              matched = None
            )
          )
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(
            LastSubmittedVerificationBatchResponsePage,
            lastSubmittedResponse
          )
          .success
          .value
          .set(
            CurrentVerificationBatchResponsePage,
            currentResponse
          )
          .success
          .value

      val result =
        service
          .recreateCurrentBatchFromUnmatchedVerifications(
            instanceId,
            userAnswers
          )
          .futureValue

      result mustBe userAnswers
      result.get(CurrentVerificationBatchResponsePage) mustBe Some(currentResponse)

      verifyNoInteractions(mockConnector)
      verifyNoInteractions(mockRepo)
    }

    "must ignore matched verifications and only recreate unmatched subcontractors" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val currentResponse =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(
            LastSubmittedVerificationBatchResponsePage,
            lastSubmittedResponse
          )
          .success
          .value

      when(
        mockConnector.getCurrentVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      )
        .thenReturn(Future.successful(currentResponse))
        .thenReturn(Future.successful(currentResponse))

      when(
        mockConnector.createVerificationBatchAndVerifications(
          any[CreateVerificationBatchAndVerificationsRequest]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          CreateVerificationBatchAndVerificationsResponse(
            verificationBatchResourceReference = 12345L
          )
        )
      )

      when(
        mockConnector.getNewestVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(newestResponseWithBatchStatus("SUBMITTED")))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val requestCaptor =
        ArgumentCaptor.forClass(
          classOf[CreateVerificationBatchAndVerificationsRequest]
        )

      service
        .recreateCurrentBatchFromUnmatchedVerifications(instanceId, userAnswers)
        .futureValue

      verify(mockConnector)
        .createVerificationBatchAndVerifications(
          requestCaptor.capture()
        )(any[HeaderCarrier])

      requestCaptor.getValue.verificationResourceReferences mustBe
        Seq(222L)

      requestCaptor.getValue.verificationResourceReferences must not contain
        333L
    }

    "must use the central unmatched logic when selecting subcontractors" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val missingVerificationNumberSubcontractor =
        SubcontractorLastVerification(
          subcontractorId = 44L,
          subbieResourceRef = Some(444L),
          subcontractorType = None,
          utr = None
        )

      val editSubcontractor =
        SubcontractorLastVerification(
          subcontractorId = 55L,
          subbieResourceRef = Some(555L),
          subcontractorType = None,
          utr = None
        )

      val verifiedSubcontractor =
        SubcontractorLastVerification(
          subcontractorId = 66L,
          subbieResourceRef = Some(666L),
          subcontractorType = None,
          utr = None
        )

      val response =
        GetLastSubmittedVerificationBatchResponse(
          scheme = None,
          subcontractors = Seq(
            missingVerificationNumberSubcontractor,
            editSubcontractor,
            verifiedSubcontractor
          ),
          verifications = Seq(
            VerificationLastVerification(
              verificationId = 3L,
              verificationBatchId = Some(1L),
              verificationResourceRef = Some(103L),
              matched = Some(" Y "),
              verificationNumber = None,
              taxTreatment = None,
              subcontractorName = None,
              subcontractorId = Some(44L),
              actionIndicator = Some(" verify ")
            ),
            VerificationLastVerification(
              verificationId = 4L,
              verificationBatchId = Some(1L),
              verificationResourceRef = Some(104L),
              matched = Some("Y"),
              verificationNumber = Some("V0000000004"),
              taxTreatment = Some("net"),
              subcontractorName = None,
              subcontractorId = Some(55L),
              actionIndicator = Some(" edit ")
            ),
            VerificationLastVerification(
              verificationId = 5L,
              verificationBatchId = Some(1L),
              verificationResourceRef = Some(105L),
              matched = Some(" y "),
              verificationNumber = Some("V0000000005"),
              taxTreatment = Some("net"),
              subcontractorName = None,
              subcontractorId = Some(66L),
              actionIndicator = Some(" verify ")
            )
          ),
          verificationBatch = None,
          submission = None
        )

      val currentResponse =
        GetCurrentVerificationBatchResponse(
          subcontractors = Nil,
          verificationBatch = None,
          verifications = Nil
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(
            LastSubmittedVerificationBatchResponsePage,
            response
          )
          .success
          .value

      when(
        mockConnector.getCurrentVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      )
        .thenReturn(Future.successful(currentResponse))
        .thenReturn(Future.successful(currentResponse))

      when(
        mockConnector.createVerificationBatchAndVerifications(
          any[CreateVerificationBatchAndVerificationsRequest]
        )(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          CreateVerificationBatchAndVerificationsResponse(
            verificationBatchResourceReference = 12345L
          )
        )
      )

      when(
        mockConnector.getNewestVerificationBatch(
          eqTo(instanceId)
        )(any[HeaderCarrier])
      ).thenReturn(Future.successful(newestResponseWithBatchStatus("SUBMITTED")))

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      val requestCaptor =
        ArgumentCaptor.forClass(
          classOf[CreateVerificationBatchAndVerificationsRequest]
        )

      service
        .recreateCurrentBatchFromUnmatchedVerifications(instanceId, userAnswers)
        .futureValue

      verify(mockConnector)
        .createVerificationBatchAndVerifications(
          requestCaptor.capture()
        )(any[HeaderCarrier])

      requestCaptor.getValue.verificationResourceReferences mustBe
        Seq(444L, 555L)

      requestCaptor.getValue.verificationResourceReferences must not contain
        666L
    }

    "must fail when LastSubmittedVerificationBatchResponsePage is missing" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      val exception =
        recoverToExceptionIf[RuntimeException] {
          service.recreateCurrentBatchFromUnmatchedVerifications(
            instanceId,
            userAnswers
          )
        }.futureValue

      exception.getMessage mustBe
        "LastSubmittedVerificationBatchResponsePage not found in session data"

      verify(mockConnector, never())
        .getCurrentVerificationBatch(
          any[String]
        )(any[HeaderCarrier])

      verify(mockConnector, never())
        .createVerificationBatchAndVerifications(
          any[CreateVerificationBatchAndVerificationsRequest]
        )(any[HeaderCarrier])

      verify(mockConnector, never())
        .modifyVerificationBatch(
          any[ModifyVerificationsRequest]
        )(any[HeaderCarrier])

      verify(mockConnector, never())
        .getNewestVerificationBatch(
          any[String]
        )(any[HeaderCarrier])

      verify(mockRepo, never())
        .set(any[UserAnswers])
    }
  }

  "VerificationService.proceedInsufficientVerification" - {

    val subcontractorId              = 123L
    val cisId                        = "cis-123"
    val verificationBatchResourceRef = 99L
    val verificationResourceRef      = 10L

    val currentBatchResponse: GetCurrentVerificationBatchResponse =
      GetCurrentVerificationBatchResponse(
        subcontractors = Seq(
          SubcontractorCurrentVerification(
            subcontractorId = subcontractorId,
            subbieResourceRef = Some(1111L),
            firstName = None,
            secondName = None,
            surname = None,
            tradingName = None,
            utr = None,
            nino = None,
            crn = None,
            partnerUtr = None,
            partnershipTradingName = None,
            subcontractorType = None,
            addressLine1 = None,
            addressLine2 = None,
            addressLine3 = None,
            addressLine4 = None,
            country = None,
            postcode = None,
            emailAddress = None,
            phoneNumber = None,
            mobilePhoneNumber = None,
            worksReferenceNumber = None,
            matched = None,
            autoVerified = None,
            verified = None,
            verificationNumber = None,
            taxTreatment = None,
            verificationDate = None,
            version = None,
            updatedTaxTreatment = None,
            lastMonthlyReturnDate = None,
            pendingVerifications = None
          )
        ),
        verificationBatch = Some(
          VerificationBatchCurrentVerification(
            verificationBatchId = 999L,
            verifBatchResourceRef = Some(verificationBatchResourceRef)
          )
        ),
        verifications = Seq(
          VerificationCurrentVerification(
            verificationId = 1L,
            verificationBatchId = Some(999L),
            subcontractorId = Some(subcontractorId),
            verificationResourceRef = Some(verificationResourceRef),
            subcontractorName = None,
            verificationNumber = None,
            taxTreatment = None,
            actionIndicator = None,
            proceed = None,
            matched = None
          )
        )
      )

    "must call the connector with the correct request when resource refs are available" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val request = ProceedVerificationRequest(
        instanceId = cisId,
        verificationBatchResourceRef = verificationBatchResourceRef,
        verificationResourceRef = verificationResourceRef
      )

      when(mockConnector.proceedInsufficientVerification(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(()))

      service.proceedInsufficientVerification(cisId, subcontractorId, currentBatchResponse).futureValue mustBe ()

      verify(mockConnector).proceedInsufficientVerification(request)
    }

    "must fail when subcontractorId is missing" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val result = service.proceedInsufficientVerification(cisId, 99, currentBatchResponse)

      result.failed.futureValue mustBe a[RuntimeException]

      verify(mockConnector, never).proceedInsufficientVerification(any[ProceedVerificationRequest])(
        any[HeaderCarrier]
      )
    }
  }

  "VerificationService.proceedUnmatchedVerification" - {

    val subcontractorId              = 123L
    val cisId                        = "cis-123"
    val verificationBatchResourceRef = 99L
    val verificationResourceRef      = 10L

    val currentBatchResponse: GetCurrentVerificationBatchResponse =
      GetCurrentVerificationBatchResponse(
        subcontractors = Seq(
          SubcontractorCurrentVerification(
            subcontractorId = subcontractorId,
            subbieResourceRef = Some(1111L),
            firstName = None,
            secondName = None,
            surname = None,
            tradingName = None,
            utr = None,
            nino = None,
            crn = None,
            partnerUtr = None,
            partnershipTradingName = None,
            subcontractorType = None,
            addressLine1 = None,
            addressLine2 = None,
            addressLine3 = None,
            addressLine4 = None,
            country = None,
            postcode = None,
            emailAddress = None,
            phoneNumber = None,
            mobilePhoneNumber = None,
            worksReferenceNumber = None,
            matched = None,
            autoVerified = None,
            verified = None,
            verificationNumber = None,
            taxTreatment = None,
            verificationDate = None,
            version = None,
            updatedTaxTreatment = None,
            lastMonthlyReturnDate = None,
            pendingVerifications = None
          )
        ),
        verificationBatch = Some(
          VerificationBatchCurrentVerification(
            verificationBatchId = 999L,
            verifBatchResourceRef = Some(verificationBatchResourceRef)
          )
        ),
        verifications = Seq(
          VerificationCurrentVerification(
            verificationId = 1L,
            verificationBatchId = Some(999L),
            subcontractorId = Some(subcontractorId),
            verificationResourceRef = Some(verificationResourceRef),
            subcontractorName = None,
            verificationNumber = None,
            taxTreatment = None,
            actionIndicator = None,
            proceed = None,
            matched = None
          )
        )
      )

    "must call the connector with the correct request when resource refs are available" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val request = ProceedVerificationRequest(
        instanceId = cisId,
        verificationBatchResourceRef = verificationBatchResourceRef,
        verificationResourceRef = verificationResourceRef
      )

      when(mockConnector.proceedUnmatchedVerification(eqTo(request))(any[HeaderCarrier]))
        .thenReturn(Future.successful(()))

      service.proceedUnmatchedVerification(cisId, subcontractorId, currentBatchResponse).futureValue mustBe ()

      verify(mockConnector).proceedUnmatchedVerification(request)
    }

    "must fail when subcontractorId is missing" in {

      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val result = service.proceedUnmatchedVerification(cisId, 99, currentBatchResponse)

      result.failed.futureValue mustBe a[RuntimeException]

      verify(mockConnector, never).proceedUnmatchedVerification(any[ProceedVerificationRequest])(
        any[HeaderCarrier]
      )
    }
  }

  "VerificationService.resetUserAnswers" - {

    "must reset UserAnswers keeping only CisIdQuery and persist them" in {
      val mockConnector     = mock[ConstructionIndustrySchemeConnector]
      val mockRepo          = mock[SessionRepository]
      val service           = buildService(mockConnector, mockRepo)
      val submissionDetails =
        VerificationSubmissionDetails(
          submissionId = "13602",
          status = "ACCEPTED",
          hmrcMarkGenerated = "hmrc-mark",
          hmrcMarkGgis = None,
          correlationId = None,
          pollUrl = None,
          pollIntervalSeconds = None,
          submittedAt = LocalDateTime.now(),
          lastMessageDate = None,
          timedOut = false
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value
          .set(VerificationSubmissionDetailsPage, submissionDetails)
          .success
          .value

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.successful(true))

      service.resetUserAnswers(userAnswers).futureValue

      val captor = ArgumentCaptor.forClass(classOf[UserAnswers])
      verify(mockRepo).set(captor.capture())

      val resetAnswers = captor.getValue

      resetAnswers.get(CisIdQuery) mustBe Some(instanceId)
      resetAnswers.get(VerificationSubmissionDetailsPage) mustBe None
    }

    "must log and gracefully recover when CisIdQuery is missing and not persist UserAnswers" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      service
        .resetUserAnswers(emptyUserAnswers)
        .futureValue

      verify(mockRepo, never()).set(any[UserAnswers])
    }

    "must silently consume session repository failure" in {
      val mockConnector = mock[ConstructionIndustrySchemeConnector]
      val mockRepo      = mock[SessionRepository]
      val service       = buildService(mockConnector, mockRepo)

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, instanceId)
          .success
          .value

      when(mockRepo.set(any[UserAnswers]))
        .thenReturn(Future.failed(new RuntimeException("session save failed")))

      service.resetUserAnswers(userAnswers).futureValue mustBe ()

      verify(mockRepo).set(any[UserAnswers])
    }
  }
}
