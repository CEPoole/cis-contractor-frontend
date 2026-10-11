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
import controllers.routes
import models.{NormalMode, Subcontractor, SubcontractorViewModel, TypeOfSubcontractor, UserAnswers}
import models.response.GetNewestVerificationBatchResponse
import models.verify.SelectedSubcontractors
import org.jsoup.Jsoup
import navigation.{FakeNavigator, Navigator}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, verify, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.verify.*
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import play.api.i18n.Messages
import repositories.SessionRepository
import viewmodels.verify.SubcontractorReverifyRow

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.Future

class SelectSubcontractorsToReverifyControllerSpec extends SpecBase with MockitoSugar {

  implicit val messages: Messages = play.api.i18n.MessagesImpl(
    play.api.i18n.Lang.defaultLang,
    app.injector.instanceOf[play.api.i18n.MessagesApi]
  )

  def onwardRoute: Call = Call("GET", "/foo")

  private val fixedClock: Clock =
    Clock.fixed(Instant.parse("2026-01-25T10:00:00Z"), ZoneOffset.UTC)

  private def newestBatchResponse(subcontractors: Seq[Subcontractor]): GetNewestVerificationBatchResponse =
    GetNewestVerificationBatchResponse(
      scheme = None,
      subcontractors = subcontractors,
      verificationBatch = None,
      verifications = Nil,
      submission = None,
      monthlyReturn = None,
      monthlyReturnSubmission = None
    )

  private def pendingUserAnswers: UserAnswers =
    emptyUserAnswers
      .set(
        NewestVerificationBatchResponsePage,
        newestBatchResponse(Nil).copy(
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

  private def mkSub(
    id: Long,
    verified: Option[String],
    firstName: Option[String] = None,
    surname: Option[String] = None,
    tradingName: Option[String] = None,
    partnershipTradingName: Option[String] = None,
    subcontractorType: Option[String] = None,
    utr: Option[String] = None,
    verificationNumber: Option[String] = None,
    taxTreatment: Option[String] = None,
    verificationDate: Option[LocalDateTime] = None,
    lastMonthlyReturnDate: Option[LocalDateTime] = None,
    createDate: Option[LocalDateTime] = None
  ): Subcontractor =
    Subcontractor(
      subcontractorId = id,
      firstName = firstName,
      secondName = None,
      surname = surname,
      tradingName = tradingName,
      partnershipTradingName = partnershipTradingName,
      verified = verified,
      verificationNumber = verificationNumber,
      taxTreatment = taxTreatment,
      verificationDate = verificationDate,
      lastMonthlyReturnDate = lastMonthlyReturnDate,
      createDate = createDate,
      subcontractorType = subcontractorType,
      subbieResourceRef = None,
      utr = utr,
      partnerUtr = None,
      crn = None,
      nino = None
    )

  private def mkVerifiedSubcontractor(
    id: Long,
    name: String
  ): Subcontractor =
    mkSub(
      id = id,
      verified = Some("Y"),
      tradingName = Some(name),
      subcontractorType = Some("company"),
      utr = Some(s"$id$id$id$id$id$id$id$id$id$id"),
      verificationDate = None,
      createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
    )

  private def url(page: Int = 1): String =
    controllers.verify.routes.SelectSubcontractorsToReverifyController
      .onPageLoad(NormalMode, page)
      .url

  private lazy val postUrl: String =
    controllers.verify.routes.SelectSubcontractorsToReverifyController
      .onPageLoad(NormalMode)
      .url

  "SelectSubcontractorsToReverifyController" - {

    "onPageLoad" - {

      "must redirect to JourneyRecovery when NewestVerificationBatchResponsePage is missing" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val app =
          applicationBuilder(userAnswers = Some(emptyUserAnswers))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val result = route(app, FakeRequest(GET, url())).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe routes.JourneyRecoveryController.onPageLoad().url

          verify(mockRepo, never()).set(any())
        }
      }

      "must redirect to verification request in progress when a verification is pending" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val app =
          applicationBuilder(userAnswers = Some(pendingUserAnswers))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val result = route(app, FakeRequest(GET, url())).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe
            controllers.verify.routes.VerificationRequestInProgressController.onPageLoad().url

          verify(mockRepo, never()).set(any())
        }
      }

      "must return OK, render rows derived from newest batch and save SubcontractorReverifyRowsPage in session" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        // Sub 1: VERIFIED=Y, verificationDate missing => reverifyRequired=true => Verified column = No,
        val subNeedsReverify =
          mkSub(
            id = 100L,
            verified = Some("Y"),
            firstName = Some("Martin"),
            surname = Some("Brody"),
            tradingName = Some("Some Trading"),
            subcontractorType = Some(TypeOfSubcontractor.Individualorsoletrader.toString),
            utr = Some("1234567890"),
            verificationDate = None,
            createDate = Some(LocalDateTime.of(2020, 5, 11, 0, 0))
          )

        // Sub 2: VERIFIED=Y, verificationDate recent => reverifyRequired=false => Verified column =Yes,
        val subNoReverify =
          mkSub(
            id = 200L,
            verified = Some("Y"),
            tradingName = Some("Hammond House"),
            subcontractorType = Some(TypeOfSubcontractor.Limitedcompany.toString),
            utr = Some("2904743750"),
            verificationNumber = Some("V0001217702"),
            taxTreatment = Some("gross"),
            verificationDate = Some(LocalDateTime.of(2025, 10, 1, 0, 0)),
            createDate = Some(LocalDateTime.of(2025, 10, 1, 0, 0))
          )

        val ua =
          emptyUserAnswers
            .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(subNeedsReverify, subNoReverify)))
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val result = route(app, FakeRequest(GET, url())).value

          status(result) mustBe OK

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue.get(SubcontractorReverifyRowsPage) mustBe defined
          uaCaptor.getValue.get(SubcontractorReverifyRowsPage).value.map(_.id) must contain allOf ("100", "200")

          val body = contentAsString(result)
          body must include("Which subcontractors do you want to reverify?")

          body must include("Brody, Martin")
          body must include("Hammond House")
          body must include("1234567890")
          body must include("2904743750")

          body must include(messages("site.no"))
          body must include(messages("site.yes"))

          body must include("V0001217702")
          body must include(messages("site.unknown"))

          body must include(messages("verify.selectSubcontractorsToReverify.taxTreatment.gross"))

          body must include("11 May 2020")
          body must include("1 Oct 2025")
        }
      }

      "must return OK and save empty SubcontractorReverifyRowsPage when there are no VERIFIED=Y subcontractors" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val ua =
          emptyUserAnswers
            .set(
              NewestVerificationBatchResponsePage,
              newestBatchResponse(
                Seq(
                  mkSub(id = 1L, verified = Some("N"), tradingName = Some("Not verified"))
                )
              )
            )
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val result = route(app, FakeRequest(GET, url())).value

          status(result) mustBe OK

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue.get(SubcontractorReverifyRowsPage) mustBe Some(Seq.empty)

          contentAsString(result) must include("Which subcontractors do you want to reverify?")
        }
      }
    }

    "onSubmit" - {

      "must redirect to verification request in progress when a verification is pending" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val app =
          applicationBuilder(userAnswers = Some(pendingUserAnswers))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val result =
            route(
              app,
              FakeRequest(POST, postUrl).withFormUrlEncodedBody("value[0]" -> "1")
            ).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe
            controllers.verify.routes.VerificationRequestInProgressController.onPageLoad().url

          verify(mockRepo, never()).set(any())
        }
      }

      "must return BadRequest and not call repo when SubcontractorReverifyRowsPage is missing (getOrElse Seq.empty path) and selection is required" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val ua =
          userAnswersWithCisId
            .set(UnverifiedSubcontractorsPage, Seq.empty)
            .success
            .value
            .set(SelectSubcontractorPage, Set.empty)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(POST, postUrl)
              .withFormUrlEncodedBody("value" -> "")

          val result = route(app, request).value

          status(result) mustBe BAD_REQUEST
          verify(mockRepo, never()).set(any())
        }
      }

      "must redirect to the next page when valid data is submitted (uses rows stored in SubcontractorReverifyRowsPage)" in {
        val mockRepo = mock[SessionRepository]

        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows: Seq[SubcontractorReverifyRow] =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Brody, Martin",
              utr = "1234567890",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "11 May 2020"
            )
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(POST, postUrl)
              .withFormUrlEncodedBody("value[0]" -> "100")

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe onwardRoute.url

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())
          uaCaptor.getValue.get(SelectSubcontractorsToReverifyPage).value must contain(
            SelectedSubcontractors("100", "Brody, Martin")
          )
        }
      }

      "must redirect to the next page when value[] contains selected subcontractors" in {
        val mockRepo = mock[SessionRepository]

        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows: Seq[SubcontractorReverifyRow] =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Brody, Martin",
              utr = "1234567890",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "11 May 2020"
            )
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(POST, postUrl)
              .withFormUrlEncodedBody(
                "value[]" -> "100"
              )

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe onwardRoute.url

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue
            .get(SelectSubcontractorsToReverifyPage)
            .value mustBe Set(
            SelectedSubcontractors("100", "Brody, Martin")
          )
        }
      }

      "must preserve selections made on page 1 when submitting page 2 with no selections" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val pageOneRows =
          (1 to 6).map { i =>
            SubcontractorReverifyRow(
              id = (100 + i).toString,
              name = f"Alpha $i%02d Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          }

        val pageTwoRow =
          SubcontractorReverifyRow(
            id = "200",
            name = "Zulu Ltd",
            utr = "",
            verified = "Yes",
            verificationNumber = "Unknown",
            taxTreatment = "Unknown",
            dateAdded = "1 Jan 2024"
          )

        val rows = pageOneRows :+ pageTwoRow

        val selectedPageOneSubcontractor =
          SelectedSubcontractors("101", "Alpha 01 Ltd")

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(
              SelectSubcontractorsToReverifyPage,
              Set(selectedPageOneSubcontractor)
            )
            .success
            .value
            .set(
              UnverifiedSubcontractorsPage,
              Seq.empty
            )
            .success
            .value
            .set(
              SelectSubcontractorPage,
              Set.empty[SubcontractorViewModel]
            )
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(
              POST,
              url(page = 2)
            ).withFormUrlEncodedBody()

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe onwardRoute.url

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue
            .get(SelectSubcontractorsToReverifyPage)
            .value mustBe Set(selectedPageOneSubcontractor)
        }
      }

      "must preserve page 1 selections when navigating to page 2" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Alpha Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            ),
            SubcontractorReverifyRow(
              id = "200",
              name = "Beta Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(
              POST,
              url(page = 1)
            ).withFormUrlEncodedBody(
              "value[0]" -> "100",
              "gotoPage" -> "2"
            )

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe url(2)

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue
            .get(SelectSubcontractorsToReverifyPage)
            .value mustBe Set(
            SelectedSubcontractors("100", "Alpha Ltd")
          )
        }
      }

      "must replace selections from the current page while preserving selections from other pages" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Alpha Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            ),
            SubcontractorReverifyRow(
              id = "200",
              name = "Beta Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          )

        val existingSelections =
          Set(
            SelectedSubcontractors("100", "Alpha Ltd"),
            SelectedSubcontractors("200", "Beta Ltd")
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(SelectSubcontractorsToReverifyPage, existingSelections)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          // Page 1 contains both rows. User deselects Alpha and keeps Beta.
          val request =
            FakeRequest(
              POST,
              url(page = 1)
            ).withFormUrlEncodedBody(
              "value[0]" -> "200"
            )

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe onwardRoute.url

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue
            .get(SelectSubcontractorsToReverifyPage)
            .value mustBe Set(
            SelectedSubcontractors("200", "Beta Ltd")
          )
        }
      }

      "must return BadRequest when submitting empty on same page that has existing session selections" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows: Seq[SubcontractorReverifyRow] =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Brody, Martin",
              utr = "1234567890",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "11 May 2020"
            )
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(
              SelectSubcontractorsToReverifyPage,
              Set(SelectedSubcontractors("100", "Brody, Martin"))
            )
            .success
            .value
            .set(UnverifiedSubcontractorsPage, Seq.empty)
            .success
            .value
            .set(SelectSubcontractorPage, Set.empty[SubcontractorViewModel])
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[Clock].toInstance(fixedClock),
              bind[SessionRepository].toInstance(mockRepo)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(POST, postUrl)
              .withFormUrlEncodedBody()

          val result = route(app, request).value

          status(result) mustBe BAD_REQUEST
          verify(mockRepo, never()).set(any())

          val body = contentAsString(result)
          body must include(messages("verify.selectSubcontractorsToReverify.error.required"))
        }
      }

      "must return BadRequest and show an error when more than 100 subcontractors are selected" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows: Seq[SubcontractorReverifyRow] =
          (1 to 101).map { i =>
            SubcontractorReverifyRow(
              id = i.toString,
              name = f"Subcontractor $i%03d Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          }

        val priorSelections: Set[SelectedSubcontractors] =
          rows
            .drop(6)
            .map(row => SelectedSubcontractors(row.id, row.name))
            .toSet

        priorSelections.size mustBe 95

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(UnverifiedSubcontractorsPage, Seq.empty)
            .success
            .value
            .set(
              SelectSubcontractorPage,
              Set.empty[SubcontractorViewModel]
            )
            .success
            .value
            .set(
              SelectSubcontractorsToReverifyPage,
              priorSelections
            )
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val formData =
            (0 until 6).map { index =>
              s"value[$index]" -> rows(index).id
            }

          val request =
            FakeRequest(
              POST,
              url(page = 1)
            ).withFormUrlEncodedBody(formData*)

          val result = route(app, request).value

          status(result) mustBe BAD_REQUEST

          val body = contentAsString(result)

          body must include(
            messages("verify.selectSubcontractorsToReverify.error.maxSelected")
          )

          verify(mockRepo, never()).set(any())
        }
      }

      "must return BadRequest when combined unverified and reverify selections exceed 100" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        // 6 rows on page 1 (page size = 6 for reverify service)
        val rows: Seq[SubcontractorReverifyRow] =
          (1 to 6).map { i =>
            SubcontractorReverifyRow(
              id = i.toString,
              name = f"Subcontractor $i%03d Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          }

        // 95 unverified subs already selected on the first page + 6 reverify = 101 combined
        val previousUnverifiedSelections: Set[SubcontractorViewModel] =
          (100 to 194).map(i => SubcontractorViewModel(i.toString, s"Unverified Sub $i")).toSet

        previousUnverifiedSelections.size mustBe 95

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(UnverifiedSubcontractorsPage, Seq.empty)
            .success
            .value
            .set(SelectSubcontractorPage, previousUnverifiedSelections)
            .success
            .value
            .set(SelectSubcontractorsToReverifyPage, Set.empty[SelectedSubcontractors])
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val formData =
            rows.indices.map { index =>
              s"value[$index]" -> rows(index).id
            }

          val request =
            FakeRequest(POST, url(page = 1))
              .withFormUrlEncodedBody(formData*)

          val result = route(app, request).value

          status(result) mustBe BAD_REQUEST

          val body = contentAsString(result)
          body must include(
            messages("verify.selectSubcontractorsToReverify.error.maxSelected")
          )

          verify(mockRepo, never()).set(any())
        }
      }

      "must allow gotoPage navigation even when more than 100 subcontractors are selected" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows: Seq[SubcontractorReverifyRow] =
          (1 to 101).map { i =>
            SubcontractorReverifyRow(
              id = i.toString,
              name = f"Subcontractor $i%03d Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          }

        val priorSelections: Set[SelectedSubcontractors] =
          rows
            .drop(6)
            .map(row => SelectedSubcontractors(row.id, row.name))
            .toSet

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(UnverifiedSubcontractorsPage, Seq.empty)
            .success
            .value
            .set(SelectSubcontractorPage, Set.empty[SubcontractorViewModel])
            .success
            .value
            .set(SelectSubcontractorsToReverifyPage, priorSelections)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val formData =
            (0 until 6).map { index =>
              s"value[$index]" -> rows(index).id
            } :+ ("gotoPage" -> "2")

          val request =
            FakeRequest(POST, url(page = 1))
              .withFormUrlEncodedBody(formData*)

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe url(2)
        }
      }

      "must allow submission when only SelectSubcontractorsToReverifyPage contains selections" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Alpha Ltd",
              utr = "",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "1 Jan 2024"
            )
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value
            .set(UnverifiedSubcontractorsPage, Seq.empty)
            .success
            .value
            .set(SelectSubcontractorPage, Set.empty[SubcontractorViewModel])
            .success
            .value
            .set(
              SelectSubcontractorsToReverifyPage,
              Set(SelectedSubcontractors("100", "Alpha Ltd"))
            )
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(POST, postUrl)
              .withFormUrlEncodedBody(
                "value[0]" -> "100"
              )

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe onwardRoute.url

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())

          uaCaptor.getValue
            .get(SelectSubcontractorsToReverifyPage)
            .value mustBe Set(
            SelectedSubcontractors("100", "Alpha Ltd")
          )
        }
      }

      "must redirect to target page when gotoPage is present and persist selections" in {
        val mockRepo = mock[SessionRepository]
        when(mockRepo.set(any())) thenReturn Future.successful(true)

        val rows: Seq[SubcontractorReverifyRow] =
          Seq(
            SubcontractorReverifyRow(
              id = "100",
              name = "Brody, Martin",
              utr = "1234567890",
              verified = "Yes",
              verificationNumber = "Unknown",
              taxTreatment = "Unknown",
              dateAdded = "11 May 2020"
            )
          )

        val ua =
          userAnswersWithCisId
            .set(SubcontractorReverifyRowsPage, rows)
            .success
            .value

        val app =
          applicationBuilder(userAnswers = Some(ua))
            .overrides(
              bind[SessionRepository].toInstance(mockRepo),
              bind[Clock].toInstance(fixedClock)
            )
            .build()

        running(app) {
          val request =
            FakeRequest(POST, url())
              .withFormUrlEncodedBody(
                "value[0]" -> "100",
                "gotoPage" -> "2"
              )

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe url(2)

          val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
          verify(mockRepo).set(uaCaptor.capture())
          uaCaptor.getValue.get(SelectSubcontractorsToReverifyPage).value must contain(
            SelectedSubcontractors("100", "Brody, Martin")
          )
        }
      }

      "must redirect to JourneyRecovery when no existing data is found (requireData fails)" in {
        val mockRepo = mock[SessionRepository]
        val app      =
          applicationBuilder(userAnswers = None)
            .overrides(bind[SessionRepository].toInstance(mockRepo))
            .build()

        running(app) {
          val request =
            FakeRequest(POST, url())
              .withFormUrlEncodedBody("value[0]" -> "100")

          val result = route(app, request).value

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe routes.JourneyRecoveryController.onPageLoad().url

          verify(mockRepo, never()).set(any())
        }
      }
    }

    "must show name as No name provided when subcontractorType is missing" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          firstName = Some("Jane"),
          surname = Some("Doe"),
          tradingName = Some("Doe Trading"),
          subcontractorType = None,
          utr = Some("9999999999"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK
        val body = contentAsString(result)

        body must include(messages("verify.noName"))
        body must include("9999999999")
      }
    }

    "must show name as surname only when a Individualorsoletrader row whose surname is provided, firstName is blank" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          firstName = Some(" "),
          surname = Some("Doe"),
          tradingName = Some("Doe Trading"),
          subcontractorType = Some("soletrader"),
          utr = Some("9999999999"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK
        val body = contentAsString(result)

        body must include("Doe")
        body must not include "Doe Trading" // confirm surname wins over tradingName
        body must include("9999999999")
      }
    }

    "must show name as tradingName when a Individualorsoletrader row whose tradingName is provided, firstName and surname are blank" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          firstName = Some(" "),
          surname = Some(" "),
          tradingName = Some("Doe Trading"),
          subcontractorType = Some("soletrader"),
          utr = Some("9999999999"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK
        val body = contentAsString(result)

        body must include("Doe Trading")
        body must include("9999999999")
      }
    }

    "must show name as tradingName when a Individualorsoletrader row whose tradingName and firstNare are provided, surname are blank" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          firstName = Some("John"),
          surname = Some(" "),
          tradingName = Some("Doe Trading"),
          subcontractorType = Some("soletrader"),
          utr = Some("9999999999"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK
        val body = contentAsString(result)

        body must include("Doe Trading")
        body must include("9999999999")
      }
    }

    "must show name as partnershipTradingName when a Partnership row whose partnershipTradingName is provided" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          partnershipTradingName = Some("ABC Partnership"),
          tradingName = Some("Doe Trading"),
          subcontractorType = Some("partnership"),
          utr = Some("9999999999"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK
        val body = contentAsString(result)

        body must include("ABC Partnership")
        body must include("9999999999")
      }
    }

    "must show name as tradingName when a Partnership row whose partnershipTradingName is blank, tradingName is provided" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          partnershipTradingName = Some("   "),
          tradingName = Some("Doe Trading"),
          subcontractorType = Some("partnership"),
          utr = Some("9999999999"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK
        val body = contentAsString(result)

        body must include("Doe Trading")
        body must include("9999999999")
      }
    }

    "must show empty utr when utr is missing" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val sub =
        mkSub(
          id = 400L,
          verified = Some("Y"),
          tradingName = Some("No UTR Ltd"),
          subcontractorType = Some("company"),
          utr = None,
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(sub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value
        status(result) mustBe OK

        val body = contentAsString(result)
        body must include("No UTR Ltd")
        body must not include messages("verify.noName")
      }
    }

    "must render the table rows sorted alphabetically by name" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val zulu =
        mkSub(
          id = 300L,
          verified = Some("Y"),
          firstName = Some("Zoe"),
          surname = Some("Zulu"),
          subcontractorType = Some(TypeOfSubcontractor.Individualorsoletrader.toString),
          utr = Some("1111111111"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val alpha =
        mkSub(
          id = 100L,
          verified = Some("Y"),
          firstName = Some("Amy"),
          surname = Some("Alpha"),
          subcontractorType = Some(TypeOfSubcontractor.Individualorsoletrader.toString),
          utr = Some("2222222222"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val middle =
        mkSub(
          id = 200L,
          verified = Some("Y"),
          firstName = Some("mike"),
          surname = Some("Middle"),
          subcontractorType = Some(TypeOfSubcontractor.Individualorsoletrader.toString),
          utr = Some("3333333333"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(zulu, alpha, middle)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK

        val body = contentAsString(result)

        val alphaName  = "Alpha, Amy"
        val middleName = "Middle, mike"
        val zuluName   = "Zulu, Zoe"

        body.indexOf(alphaName)  must be < body.indexOf(middleName)
        body.indexOf(middleName) must be < body.indexOf(zuluName)
      }
    }

    "must map taxTreatment to the correct display value when reverification is NOT required" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      def verifiedNoReverify(id: Long, tradingName: String, tax: Option[String]) =
        mkSub(
          id = id,
          verified = Some("Y"),
          tradingName = Some(tradingName),
          subcontractorType = Some("company"),
          utr = Some(s"$id$id$id$id$id$id$id$id$id$id"),
          verificationNumber = Some(s"V$id"),
          taxTreatment = tax,
          verificationDate = Some(LocalDateTime.of(2025, 10, 1, 0, 0)),
          createDate = Some(LocalDateTime.of(2025, 10, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(
            NewestVerificationBatchResponsePage,
            newestBatchResponse(
              Seq(
                verifiedNoReverify(1L, "Net Ltd", Some("net")),
                verifiedNoReverify(2L, "Unmatched Ltd", Some("unmatched")),
                verifiedNoReverify(3L, "Gross Ltd", Some("gross")),
                verifiedNoReverify(4L, "Unknown Ltd", Some("something-else"))
              )
            )
          )
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value
        status(result) mustBe OK

        val body = contentAsString(result)

        body must include(messages("verify.selectSubcontractorsToReverify.taxTreatment.net"))
        body must include(messages("verify.selectSubcontractorsToReverify.taxTreatment.unmatched"))
        body must include(messages("verify.selectSubcontractorsToReverify.taxTreatment.gross"))
        body must include(messages("site.unknown"))
      }
    }

    "must use partnershipTradingName (or tradingName fallback) when subcontractorType is partnership" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val partnershipWithPTN =
        mkSub(
          id = 500L,
          verified = Some("Y"),
          tradingName = Some("Trading Fallback"),
          partnershipTradingName = Some("Partnership Trading Name"),
          subcontractorType = Some("partnership"),
          utr = Some("5555555555"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val partnershipWithoutPTN =
        mkSub(
          id = 600L,
          verified = Some("Y"),
          tradingName = Some("Trading Only"),
          partnershipTradingName = None,
          subcontractorType = Some("partnership"),
          utr = Some("6666666666"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(partnershipWithPTN, partnershipWithoutPTN)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value
        status(result) mustBe OK

        val body = contentAsString(result)

        body must include("Partnership Trading Name")
        body must include("Trading Only")
      }
    }

    "must pre-populate the form (checked boxes) from SelectSubcontractorsToReverifyPage using ids" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val a =
        mkSub(
          id = 700L,
          verified = Some("Y"),
          tradingName = Some("A Ltd"),
          subcontractorType = Some("company"),
          utr = Some("7007007007"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val b =
        mkSub(
          id = 800L,
          verified = Some("Y"),
          tradingName = Some("B Ltd"),
          subcontractorType = Some("company"),
          utr = Some("8008008008"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(a, b)))
          .success
          .value
          .set(
            SelectSubcontractorsToReverifyPage,
            Set(
              SelectedSubcontractors("700", "A Ltd")
            )
          )
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value
        status(result) mustBe OK

        val body = contentAsString(result)

        val doc = org.jsoup.Jsoup.parse(body)

        val input700 =
          doc.selectFirst("input[value=700]")

        input700 must not be null
        input700.hasAttr("checked") mustBe true
      }
    }

    "must only render and persist rows for subcontractors that are already verified" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val verifiedSub =
        mkSub(
          id = 100L,
          verified = Some("Y"),
          tradingName = Some("Verified Ltd"),
          subcontractorType = Some("company"),
          utr = Some("1111111111"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val unverifiedSub =
        mkSub(
          id = 200L,
          verified = Some("N"),
          tradingName = Some("Unverified Ltd"),
          subcontractorType = Some("company"),
          utr = Some("2222222222"),
          verificationDate = None,
          createDate = Some(LocalDateTime.of(2024, 1, 1, 0, 0))
        )

      val ua =
        emptyUserAnswers
          .set(NewestVerificationBatchResponsePage, newestBatchResponse(Seq(verifiedSub, unverifiedSub)))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val result = route(app, FakeRequest(GET, url())).value

        status(result) mustBe OK

        val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockRepo).set(uaCaptor.capture())

        val savedRows = uaCaptor.getValue.get(SubcontractorReverifyRowsPage).value
        savedRows.map(_.id) mustBe Seq("100")

        val body = contentAsString(result)
        body must include("Verified Ltd")
        body must not include "Unverified Ltd"
      }
    }

    "must return BadRequest when rows exist, selection is required and no checkbox is selected" in {
      val mockRepo = mock[SessionRepository]
      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val rows: Seq[SubcontractorReverifyRow] =
        Seq(
          SubcontractorReverifyRow(
            id = "100",
            name = "Brody, Martin",
            utr = "1234567890",
            verified = "Yes",
            verificationNumber = "Unknown",
            taxTreatment = "Unknown",
            dateAdded = "11 May 2020"
          )
        )

      val ua =
        userAnswersWithCisId
          .set(SubcontractorReverifyRowsPage, rows)
          .success
          .value
          .set(UnverifiedSubcontractorsPage, Seq.empty)
          .success
          .value
          .set(SelectSubcontractorPage, Set.empty[SubcontractorViewModel])
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[Clock].toInstance(fixedClock),
            bind[SessionRepository].toInstance(mockRepo)
          )
          .build()

      running(app) {
        val request =
          FakeRequest(POST, postUrl)
            .withFormUrlEncodedBody()

        val result = route(app, request).value

        status(result) mustBe BAD_REQUEST
        verify(mockRepo, never()).set(any())

        val body = contentAsString(result)
        body must include(messages("verify.selectSubcontractorsToReverify.error.required"))
      }
    }

    "must allow empty submission when UnverifiedSubcontractorsPage is non-empty" in {
      val mockRepo = mock[SessionRepository]

      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val rows: Seq[SubcontractorReverifyRow] =
        Seq(
          SubcontractorReverifyRow(
            id = "100",
            name = "Brody, Martin",
            utr = "1234567890",
            verified = "Yes",
            verificationNumber = "Unknown",
            taxTreatment = "Unknown",
            dateAdded = "11 May 2020"
          )
        )

      val unverified =
        Seq(
          mkSub(
            id = 999L,
            verified = Some("N"),
            tradingName = Some("Pending Verification Ltd"),
            subcontractorType = Some("company")
          )
        )

      val ua =
        userAnswersWithCisId
          .set(SubcontractorReverifyRowsPage, rows)
          .success
          .value
          .set(UnverifiedSubcontractorsPage, unverified)
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[SessionRepository].toInstance(mockRepo),
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[Clock].toInstance(fixedClock)
          )
          .build()

      running(app) {
        val request =
          FakeRequest(POST, postUrl)
            .withFormUrlEncodedBody()

        val result = route(app, request).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value mustBe onwardRoute.url

        val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockRepo).set(uaCaptor.capture())
        uaCaptor.getValue.get(SelectSubcontractorsToReverifyPage).value mustBe Set.empty
      }
    }

    "must allow empty submission when SelectSubcontractorPage already contains selections" in {
      val mockRepo = mock[SessionRepository]

      when(mockRepo.set(any())) thenReturn Future.successful(true)

      val rows: Seq[SubcontractorReverifyRow] =
        Seq(
          SubcontractorReverifyRow(
            id = "100",
            name = "Brody, Martin",
            utr = "1234567890",
            verified = "Yes",
            verificationNumber = "Unknown",
            taxTreatment = "Unknown",
            dateAdded = "11 May 2020"
          )
        )

      val ua =
        userAnswersWithCisId
          .set(SubcontractorReverifyRowsPage, rows)
          .success
          .value
          .set(SelectSubcontractorPage, Set(SubcontractorViewModel("1", "Earlier Selected Sub")))
          .success
          .value

      val app =
        applicationBuilder(userAnswers = Some(ua))
          .overrides(
            bind[SessionRepository].toInstance(mockRepo),
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[Clock].toInstance(fixedClock)
          )
          .build()

      running(app) {
        val request =
          FakeRequest(POST, postUrl)
            .withFormUrlEncodedBody()

        val result = route(app, request).value

        status(result) mustBe SEE_OTHER
        redirectLocation(result).value mustBe onwardRoute.url

        val uaCaptor = ArgumentCaptor.forClass(classOf[UserAnswers])
        verify(mockRepo).set(uaCaptor.capture())
        uaCaptor.getValue.get(SelectSubcontractorsToReverifyPage).value mustBe Set.empty
      }
    }
  }
}
