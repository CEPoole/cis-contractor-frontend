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
import forms.verify.SelectSubcontractorFormProvider
import models.response.GetNewestVerificationBatchResponse
import models.{CheckMode, NormalMode, Subcontractor, SubcontractorViewModel, UserAnswers, Verification}
import navigation.{FakeNavigator, Navigator}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{verify, verifyNoInteractions, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.verify.{NewestVerificationBatchResponsePage, RebuildVerificationFromWarningPage, SelectSubcontractorPage, UnverifiedSubcontractorsPage}
import play.api.data.Forms.*
import play.api.data.Form
import play.api.i18n.Messages
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import services.*
import uk.gov.hmrc.http.HeaderCarrier
import views.html.verify.SelectSubcontractorView
import models.agent.AgentClientData

import javax.inject.Inject
import scala.concurrent.Future

class SelectSubcontractorControllerSpec extends SpecBase with MockitoSugar {

  implicit val messages: Messages = play.api.i18n.MessagesImpl(
    play.api.i18n.Lang.defaultLang,
    app.injector.instanceOf[play.api.i18n.MessagesApi]
  )

  def onwardRoute: Call = Call("GET", "/foo")

  val formProvider            = new SelectSubcontractorFormProvider()
  val form: Form[Set[String]] = formProvider()
  val paginationService       = new PaginationService()

  def url(page: Int = 1): String =
    controllers.verify.routes.SelectSubcontractorController.onPageLoad(NormalMode, page).url

  def checkModeUrl(page: Int = 1): String =
    controllers.verify.routes.SelectSubcontractorController.onPageLoad(CheckMode, page).url

  private def generateSubcontractors(count: Int): Seq[Subcontractor] =
    (1 to count).map { subcontractorId =>
      Subcontractor(
        subcontractorId = subcontractorId.toLong,
        firstName = None,
        secondName = None,
        surname = None,
        tradingName = Some("ABC Construction Ltd"),
        partnershipTradingName = None,
        verified = None,
        verificationNumber = None,
        taxTreatment = None,
        verificationDate = None,
        lastMonthlyReturnDate = None,
        createDate = None,
        subcontractorType = Some("company"),
        subbieResourceRef = Some(9999L),
        utr = None,
        partnerUtr = None,
        crn = None,
        nino = None
      )
    }

  private val subcontractorCount = 10

  private val subcontractors: Seq[Subcontractor] = generateSubcontractors(subcontractorCount)

  private val getNewestVerificationBatchResponse: GetNewestVerificationBatchResponse =
    GetNewestVerificationBatchResponse(
      scheme = None,
      subcontractors = subcontractors,
      verificationBatch = None,
      verifications = Seq(
        Verification(
          verificationId = 10L,
          matched = None,
          verificationNumber = None,
          taxTreatment = None,
          verificationBatchId = None,
          subcontractorId = None
        )
      ),
      submission = None,
      monthlyReturn = None,
      monthlyReturnSubmission = None
    )

  private def uaWithSubcontractors: UserAnswers =
    userAnswersWithCisId
      .set(NewestVerificationBatchResponsePage, getNewestVerificationBatchResponse)
      .success
      .value
      .set(UnverifiedSubcontractorsPage, subcontractors)
      .success
      .value

  private def uaWithPendingVerificationRequest: UserAnswers =
    userAnswersWithCisId
      .set(
        NewestVerificationBatchResponsePage,
        getNewestVerificationBatchResponse.copy(
          verificationBatch = Some(
            models.VerificationBatch(
              verificationBatchId = 1L,
              status = Some("PENDING"),
              verificationNumber = Some("VB123")
            )
          )
        )
      )
      .success
      .value
      .set(UnverifiedSubcontractorsPage, subcontractors)
      .success
      .value

  private val allSubs          = SubcontractorViewModel.fromSubcontractors(subcontractors)
  private val brodyMartin      = allSubs.head
  private val epsilonCarpentry = allSubs(6)

  "SelectSubcontractor Controller" - {

    "must return OK and correct view for GET (page 1)" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any()))
        .thenReturn(Future.successful(true))

      val mockVerificationPreSelectionService =
        mock[VerificationPreSelectionService]

      when(
        mockVerificationPreSelectionService.preSelectedSubcontractorIds(
          any[Seq[Subcontractor]],
          any[UserAnswers]
        )
      ).thenReturn(Set.empty[String])

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(
            bind[SessionRepository].toInstance(mockSessionRepository),
            bind[VerificationPreSelectionService]
              .toInstance(mockVerificationPreSelectionService)
          )
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        val view = application.injector.instanceOf[SelectSubcontractorView]

        val allItems =
          SubcontractorViewModel.checkboxItems(allSubs)

        val paginationResult =
          paginationService.paginateCheckboxItems(allItems, 1)

        status(result) mustEqual OK

        contentAsString(result) mustEqual view(
          form,
          NormalMode,
          paginationResult.paginatedData,
          paginationResult.paginationViewModel,
          1,
          paginationResult.startIndex,
          paginationResult.totalCount,
          paginationResult.totalPages
        )(request, messages(application)).toString

        verify(mockVerificationPreSelectionService)
          .preSelectedSubcontractorIds(
            any[Seq[Subcontractor]],
            any[UserAnswers]
          )
      }
    }

    "must redirect to verification request in progress when GET is URL hopped during a pending verification" in {
      val application = applicationBuilder(userAnswers = Some(uaWithPendingVerificationRequest)).build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.verify.routes.VerificationRequestInProgressController.onPageLoad().url
      }
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers =
        uaWithSubcontractors
          .set(SelectSubcontractorPage, allSubs.toSet)
          .success
          .value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        val view = application.injector.instanceOf[SelectSubcontractorView]

        val allItems         = SubcontractorViewModel.checkboxItems(allSubs)
        val paginationResult = paginationService.paginateCheckboxItems(allItems, 1)

        status(result) mustEqual OK

        contentAsString(result) mustEqual view(
          form.fill(allSubs.map(_.id).toSet),
          NormalMode,
          paginationResult.paginatedData,
          paginationResult.paginationViewModel,
          1,
          paginationResult.startIndex,
          paginationResult.totalCount,
          paginationResult.totalPages
        )(request, messages(application)).toString
      }
    }

    "must redirect to the next page when valid data is submitted" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("value[0]" -> allSubs.head.id)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "must redirect to verification request in progress when POST is URL hopped during a pending verification" in {
      val application = applicationBuilder(userAnswers = Some(uaWithPendingVerificationRequest)).build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("value[0]" -> allSubs.head.id)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.verify.routes.VerificationRequestInProgressController.onPageLoad().url
      }
    }

    "must return BadRequest with inline error when selection is mandatory and no values are selected" in {

      val application = applicationBuilder(userAnswers = Some(uaWithSubcontractors)).build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody()

        val view = application.injector.instanceOf[SelectSubcontractorView]

        val allItems         = SubcontractorViewModel.checkboxItems(allSubs)
        val paginationResult = paginationService.paginateCheckboxItems(allItems, 1)

        val result = route(application, request).value

        val expectedForm =
          form
            .fill(Set.empty[String])
            .withError("value", "verify.selectSubcontractor.error.required")

        status(result) mustEqual BAD_REQUEST

        contentAsString(result) mustEqual view(
          expectedForm,
          NormalMode,
          paginationResult.paginatedData,
          paginationResult.paginationViewModel,
          1,
          paginationResult.startIndex,
          paginationResult.totalCount,
          paginationResult.totalPages
        )(request, messages(application)).toString
      }
    }

    "must return BadRequest when more than 100 subcontractors are selected" in {

      val manySubcontractors = generateSubcontractors(201)

      val manySubs =
        SubcontractorViewModel.fromSubcontractors(manySubcontractors)

      val allItems =
        SubcontractorViewModel.checkboxItems(manySubs)

      val firstPage =
        paginationService.paginateCheckboxItems(allItems, 1)

      val currentPageIds: Set[String] =
        firstPage.paginatedData.map(_.value).toSet

      val priorSelections: Set[SubcontractorViewModel] =
        manySubs
          .filterNot(sub => currentPageIds.contains(sub.id))
          .take(100)
          .toSet

      priorSelections.size mustBe 100

      val newlySelectedId =
        firstPage.paginatedData.head.value

      val responseWithManySubcontractors =
        getNewestVerificationBatchResponse.copy(
          subcontractors = manySubcontractors
        )

      val userAnswers =
        userAnswersWithCisId
          .set(
            NewestVerificationBatchResponsePage,
            responseWithManySubcontractors
          )
          .success
          .value
          .set(
            UnverifiedSubcontractorsPage,
            manySubcontractors
          )
          .success
          .value
          .set(
            SelectSubcontractorPage,
            priorSelections
          )
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody(
              "value[0]" -> newlySelectedId
            )

        val result = route(application, request).value

        status(result) mustBe BAD_REQUEST

        val body = contentAsString(result)

        body must include(
          "You can only include up to 100 subcontractors in a verification request"
        )
      }
    }

    "must support pagination (page 2)" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request = FakeRequest(GET, url(2))
        val result  = route(application, request).value

        status(result) mustEqual OK
      }
    }

    "must render the page for a GET when no existing data is found" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      val request = FakeRequest(GET, url())
      val result  = route(application, request).value

      status(result) mustBe OK
    }

    "must return BadRequest for POST when no existing data and form has errors" in {

      val application = applicationBuilder(userAnswers = Some(uaWithSubcontractors)).build()

      val request =
        FakeRequest(POST, url())
          .withFormUrlEncodedBody("value" -> "")

      val result = route(application, request).value

      status(result) mustBe BAD_REQUEST
    }

    "must allow gotoPage navigation even when more than 100 subcontractors are selected" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any()))
        .thenReturn(Future.successful(true))

      val manySubcontractors = generateSubcontractors(201)

      val manySubs =
        SubcontractorViewModel.fromSubcontractors(manySubcontractors)

      val allItems =
        SubcontractorViewModel.checkboxItems(manySubs)

      val firstPage =
        paginationService.paginateCheckboxItems(allItems, 1)

      val currentPageIds: Set[String] =
        firstPage.paginatedData.map(_.value).toSet

      val priorSelections: Set[SubcontractorViewModel] =
        manySubs
          .filterNot(sub => currentPageIds.contains(sub.id))
          .take(100)
          .toSet

      val responseWithManySubcontractors =
        getNewestVerificationBatchResponse.copy(
          subcontractors = manySubcontractors
        )

      val userAnswers =
        userAnswersWithCisId
          .set(NewestVerificationBatchResponsePage, responseWithManySubcontractors)
          .success
          .value
          .set(UnverifiedSubcontractorsPage, manySubcontractors)
          .success
          .value
          .set(SelectSubcontractorPage, priorSelections)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val newlySelectedId = firstPage.paginatedData.head.value

        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody(
              "value[0]" -> newlySelectedId,
              "gotoPage" -> "2"
            )

        val result = route(application, request).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value mustEqual url(2)
      }
    }

    "must initialise verification data and render the page for a GET if no existing data is found" in {

      val mockVerificationService = mock[VerificationService]
      val mockCisManageService    = mock[CisManageService]
      val mockSessionRepository   = mock[SessionRepository]

      when(
        mockCisManageService
          .ensureCisIdInUserAnswers(any[UserAnswers])(any[HeaderCarrier])
      ).thenReturn(Future.successful(emptyUserAnswers))

      when(mockSessionRepository.set(any()))
        .thenReturn(Future.successful(true))

      when(
        mockVerificationService
          .refreshNewestVerificationBatch(any[UserAnswers])(any[HeaderCarrier])
      ).thenReturn(Future.successful(uaWithSubcontractors))

      val application =
        applicationBuilder(userAnswers = None)
          .overrides(
            bind[VerificationService].toInstance(mockVerificationService),
            bind[CisManageService].toInstance(mockCisManageService),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual OK

        verify(mockCisManageService)
          .ensureCisIdInUserAnswers(any[UserAnswers])(any[HeaderCarrier])

        verify(mockVerificationService)
          .refreshNewestVerificationBatch(any[UserAnswers])(any[HeaderCarrier])
      }
    }

    "must not initialise verification data when existing session data is available" in {

      val mockVerificationService = mock[VerificationService]
      val mockCisManageService    = mock[CisManageService]

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(
            bind[VerificationService].toInstance(mockVerificationService),
            bind[CisManageService].toInstance(mockCisManageService)
          )
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual OK

        verifyNoInteractions(mockVerificationService)
        verifyNoInteractions(mockCisManageService)
      }
    }

    "must redirect to Journey Recovery when initialising verification data fails" in {

      val mockVerificationService = mock[VerificationService]
      val mockCisManageService    = mock[CisManageService]

      when(
        mockCisManageService
          .ensureCisIdInUserAnswers(any[UserAnswers])(any[HeaderCarrier])
      ).thenReturn(Future.failed(new RuntimeException("failed to initialise CIS ID")))

      val application =
        applicationBuilder(userAnswers = None)
          .overrides(
            bind[VerificationService].toInstance(mockVerificationService),
            bind[CisManageService].toInstance(mockCisManageService)
          )
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must initialise verification data and render the page for an authorised agent with no existing data" in {

      val mockVerificationService = mock[VerificationService]
      val mockCisManageService    = mock[CisManageService]
      val mockSessionRepository   = mock[SessionRepository]

      val ton = "754"
      val tor = "EZ10800"

      when(
        mockCisManageService
          .getAgentClient(any[String])(any[HeaderCarrier])
      ).thenReturn(
        Future.successful(
          Some(
            AgentClientData(
              uniqueId = "client-instance-id",
              taxOfficeNumber = ton,
              taxOfficeReference = tor,
              schemeName = None
            )
          )
        )
      )

      when(
        mockCisManageService
          .hasClient(eqTo(ton), eqTo(tor))(any[HeaderCarrier])
      ).thenReturn(Future.successful(true))

      when(mockSessionRepository.set(any()))
        .thenReturn(Future.successful(true))

      when(
        mockVerificationService
          .refreshNewestVerificationBatch(any[UserAnswers])(any[HeaderCarrier])
      ).thenReturn(Future.successful(uaWithSubcontractors))

      val application =
        applicationBuilder(
          userAnswers = None,
          isAgent = true
        )
          .overrides(
            bind[VerificationService].toInstance(mockVerificationService),
            bind[CisManageService].toInstance(mockCisManageService),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual OK

        verify(mockCisManageService)
          .getAgentClient(any[String])(any[HeaderCarrier])

        verify(mockCisManageService)
          .hasClient(eqTo(ton), eqTo(tor))(any[HeaderCarrier])

        verify(mockVerificationService)
          .refreshNewestVerificationBatch(any[UserAnswers])(any[HeaderCarrier])
      }
    }

    "must redirect to NoSubcontractorsAdded on GET when subcontractors list is empty" in {

      val getNewestVerificationBatchResponse: GetNewestVerificationBatchResponse =
        GetNewestVerificationBatchResponse(
          scheme = None,
          subcontractors = Seq.empty[Subcontractor],
          verificationBatch = None,
          verifications = Seq(
            Verification(
              verificationId = 10L,
              matched = None,
              verificationNumber = None,
              taxTreatment = None,
              verificationBatchId = None,
              subcontractorId = None
            )
          ),
          submission = None,
          monthlyReturn = None,
          monthlyReturnSubmission = None
        )

      def uaWithNoSubcontractors: UserAnswers =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, getNewestVerificationBatchResponse)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(uaWithNoSubcontractors))
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.verify.routes.NoSubcontractorsAddedController.onPageLoad().url
      }
    }

    "must redirect to VerifyYourSubcontractorsYesNo on GET when subcontractors list is not empty, unverifiedSubcontractors is empty" in {

      val subcontractors: Seq[Subcontractor] = Seq.empty[Subcontractor]

      def uaWithNoUnverifiedSubcontractors: UserAnswers =
        uaWithSubcontractors
          .set(UnverifiedSubcontractorsPage, subcontractors)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(uaWithNoUnverifiedSubcontractors))
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.verify.routes.VerifyYourSubcontractorsYesNoController.onPageLoad.url
      }
    }

    "must redirect to Journey Recovery on GET when NewestVerificationBatchResponse is not found" in {

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery on GET when UnverifiedSubcontractors is not found" in {

      def uaWithNoUnverifiedSubcontractors: UserAnswers =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, getNewestVerificationBatchResponse)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(uaWithNoUnverifiedSubcontractors))
          .build()

      running(application) {
        val request = FakeRequest(GET, url())
        val result  = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if no existing data is found" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = None)
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("value[0]" -> allSubs.head.id)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if UnverifiedSubcontractorsPage data is not found" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(userAnswersWithCisId))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("value[0]" -> allSubs.head.id)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if UnverifiedSubcontractors is empty" in {

      val subcontractorCount = 0

      def uaWithNoUnverifiedSubcontractor: UserAnswers =
        userAnswersWithCisId
          .set(UnverifiedSubcontractorsPage, generateSubcontractors(subcontractorCount))
          .success
          .value

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithNoUnverifiedSubcontractor))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("value[0]" -> allSubs.head.id)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to target page and save selections when gotoPage field is present" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody(
              "value[0]" -> allSubs.head.id,
              "gotoPage" -> "2"
            )

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual url(2)
      }
    }

    "must redirect when gotoPage is present even if no selections exist anywhere" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithSubcontractors))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("gotoPage" -> "2")

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual url(2)

        val captor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockSessionRepository).set(captor.capture())
        captor.getValue.get(SelectSubcontractorPage).value mustEqual Set.empty[SubcontractorViewModel]
      }
    }

    "must redirect to target page when gotoPage is present and there are prior selections (even if current page submits none)" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val priorSelection: Set[SubcontractorViewModel] = Set(brodyMartin)
      val userAnswers                                 =
        uaWithSubcontractors
          .set(SelectSubcontractorPage, priorSelection)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("gotoPage" -> "2")

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual url(2)
      }
    }

    "must preserve page 1 selections when submitting page 2 with no selections" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val allItems = SubcontractorViewModel.checkboxItems(allSubs)

      val page1 =
        paginationService.paginateCheckboxItems(allItems, 1)

      val page1Subcontractor =
        allSubs.find(_.id == page1.paginatedData.head.value).value

      val userAnswers =
        uaWithSubcontractors
          .set(SelectSubcontractorPage, Set(page1Subcontractor))
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {

        val request =
          FakeRequest(POST, url(2))
            .withFormUrlEncodedBody()

        val result = route(application, request).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value mustBe onwardRoute.url

        val captor: ArgumentCaptor[UserAnswers] =
          ArgumentCaptor.forClass(classOf[UserAnswers])

        verify(mockSessionRepository).set(captor.capture())

        captor.getValue
          .get(SelectSubcontractorPage)
          .value mustBe Set(page1Subcontractor)
      }
    }

    "must clear RebuildVerificationFromWarningPage after it is consumed in CheckMode" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val userAnswers =
        uaWithSubcontractors
          .set(RebuildVerificationFromWarningPage, true)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {

        val request =
          FakeRequest(POST, checkModeUrl())
            .withFormUrlEncodedBody("value[0]" -> allSubs.head.id)

        val result =
          route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url

        val captor: ArgumentCaptor[UserAnswers] =
          ArgumentCaptor.forClass(classOf[UserAnswers])

        verify(mockSessionRepository).set(captor.capture())

        captor.getValue.get(SelectSubcontractorPage).value mustEqual Set(allSubs.head)
        captor.getValue.get(RebuildVerificationFromWarningPage) mustEqual None
      }
    }

    "must preserve selections from other pages when submitting from a given page" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val page1Selection: Set[SubcontractorViewModel] = Set(brodyMartin)
      val userAnswers                                 =
        uaWithSubcontractors
          .set(SelectSubcontractorPage, page1Selection)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, url(2))
            .withFormUrlEncodedBody("value[0]" -> epsilonCarpentry.id)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url

        val captor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockSessionRepository).set(captor.capture())
        captor.getValue.get(SelectSubcontractorPage).value mustEqual
          (page1Selection + epsilonCarpentry)
      }
    }

    "must merge newly selected values with otherPageValues when mergedValues is empty and no gotoPage is provided (fold success path)" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val allItems = SubcontractorViewModel.checkboxItems(allSubs)
      val page1    = paginationService.paginateCheckboxItems(allItems, 1)
      val page2    = paginationService.paginateCheckboxItems(allItems, 2)

      val page1Subs: Set[SubcontractorViewModel] =
        page1.paginatedData.flatMap(item => allSubs.find(_.id == item.value)).toSet

      val page2Subs: Set[SubcontractorViewModel] =
        page2.paginatedData.flatMap(item => allSubs.find(_.id == item.value)).toSet

      page2Subs.nonEmpty mustBe true

      val otherPageSelection: Set[SubcontractorViewModel] = Set(page2Subs.head)

      val userAnswers =
        uaWithSubcontractors
          .set(SelectSubcontractorPage, otherPageSelection)
          .success
          .value

      val appWithAnswers =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(appWithAnswers) {

        val newSelectionOnPage1: SubcontractorViewModel =
          page1Subs.diff(otherPageSelection).head

        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody("value[0]" -> newSelectionOnPage1.id)

        val result = route(appWithAnswers, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url

        val captor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockSessionRepository).set(captor.capture())

        captor.getValue.get(SelectSubcontractorPage).value mustEqual
          (otherPageSelection + newSelectionOnPage1)
      }
    }

    "must redirect when form binds but submitted ids match no known subcontractor and selection is not mandatory" in {

      class TestSelectSubcontractorFormProvider @Inject() () extends SelectSubcontractorFormProvider {
        override def apply(): Form[Set[String]] =
          Form(single("value" -> ignored(Set("non-existent-id"))))
      }

      val subcontractorsWithVerifiedInBatch =
        subcontractors.updated(
          0,
          subcontractors.head.copy(verified = Some("Y"))
        )

      val responseWithVerified =
        getNewestVerificationBatchResponse.copy(
          subcontractors = subcontractorsWithVerifiedInBatch
        )

      val userAnswers =
        userAnswersWithCisId
          .set(NewestVerificationBatchResponsePage, responseWithVerified)
          .success
          .value
          .set(UnverifiedSubcontractorsPage, subcontractors)
          .success
          .value

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(userAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository),
            bind[SelectSubcontractorFormProvider].to[TestSelectSubcontractorFormProvider]
          )
          .build()

      running(application) {
        val request = FakeRequest(POST, url()).withFormUrlEncodedBody()

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url

        val captor: ArgumentCaptor[UserAnswers] = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockSessionRepository).set(captor.capture())

        captor.getValue.get(SelectSubcontractorPage).value mustEqual Set.empty[SubcontractorViewModel]
      }
    }

    "must allow empty submission when verified subcontractors exist" in {

      val subcontractorsWithVerified =
        subcontractors.updated(0, subcontractors.head.copy(verified = Some("Y")))

      val responseWithVerified =
        getNewestVerificationBatchResponse.copy(
          subcontractors = subcontractorsWithVerified
        )

      val ua =
        userAnswersWithCisId
          .set(NewestVerificationBatchResponsePage, responseWithVerified)
          .success
          .value
          .set(UnverifiedSubcontractorsPage, subcontractorsWithVerified)
          .success
          .value

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {

        val request =
          FakeRequest(POST, url())
            .withFormUrlEncodedBody()

        val result = route(application, request).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value mustBe onwardRoute.url
      }
    }
  }
}
