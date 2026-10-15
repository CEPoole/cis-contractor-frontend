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

import base.SpecBase
import controllers.routes
import forms.add.SubAddressYesNoFormProvider
import models.add.SubcontractorName
import models.{NormalMode, UserAnswers}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.add.{SubAddressYesNoPage, SubcontractorNamePage}
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import views.html.add.SubAddressYesNoView

import scala.concurrent.Future

class SubAddressYesNoControllerSpec extends SpecBase with MockitoSugar {

  private val formProvider = new SubAddressYesNoFormProvider()
  private val form         = formProvider()

  private lazy val subAddressYesNoRoute = controllers.add.routes.SubAddressYesNoController.onPageLoad(NormalMode).url

  private val subcontractorName = SubcontractorName("John", Some("Paul"), "Smith")

  private val name = "John Smith"

  private def uaWithName: UserAnswers =
    emptyUserAnswers.set(SubcontractorNamePage, subcontractorName).success.value

  "SubAddressYesNo Controller" - {

    "must return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(uaWithName)).build()

      running(application) {
        val request = FakeRequest(GET, subAddressYesNoRoute)

        val result = route(application, request).value

        val view = application.injector.instanceOf[SubAddressYesNoView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form, NormalMode, name)(request, messages(application)).toString
      }
    }

    "must populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = uaWithName.set(SubAddressYesNoPage, true).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, subAddressYesNoRoute)

        val view = application.injector.instanceOf[SubAddressYesNoView]

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) mustEqual view(form.fill(true), NormalMode, name)(
          request,
          messages(application)
        ).toString
      }
    }

    "must redirect to the AddressOfSubcontractor page when valid data with value Yes is submitted" in {

      val mockSessionRepository = mock[SessionRepository]

      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithName))
          .overrides(
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, subAddressYesNoRoute)
            .withFormUrlEncodedBody(("value", "true"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.add.routes.AddressOfSubcontractorController
          .redirectToAddressLookup(NormalMode, None)
          .url
      }
    }

    "must redirect to the AddIndividualContactMethodsYesNo page when valid data with value No is submitted" in {

      val mockSessionRepository = mock[SessionRepository]

      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(uaWithName))
          .overrides(
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, subAddressYesNoRoute)
            .withFormUrlEncodedBody(("value", "false"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual controllers.add.routes.AddIndividualContactMethodsYesNoController
          .onPageLoad(NormalMode)
          .url
      }
    }

    "must return a Bad Request and errors when invalid data is submitted (name present)" in {

      val application = applicationBuilder(userAnswers = Some(uaWithName)).build()

      running(application) {
        val request =
          FakeRequest(POST, subAddressYesNoRoute)
            .withFormUrlEncodedBody(("value", ""))

        val boundForm = form.bind(Map("value" -> ""))

        val view = application.injector.instanceOf[SubAddressYesNoView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) mustEqual view(boundForm, NormalMode, name)(
          request,
          messages(application)
        ).toString
      }
    }

    "must redirect to Journey Recovery for a GET if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val request = FakeRequest(GET, subAddressYesNoRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val request =
          FakeRequest(POST, subAddressYesNoRoute)
            .withFormUrlEncodedBody(("value", "true"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must return a Bad Request and errors when no value is submitted" in {

      val application = applicationBuilder(userAnswers = Some(uaWithName)).build()

      running(application) {
        val request =
          FakeRequest(POST, subAddressYesNoRoute)
            .withFormUrlEncodedBody()

        val form      = new SubAddressYesNoFormProvider()()
        val boundForm = form.bind(Map.empty)

        val view = application.injector.instanceOf[SubAddressYesNoView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) mustEqual view(boundForm, NormalMode, name)(
          request,
          messages(application)
        ).toString

        contentAsString(result) must include(messages(application)("subAddressYesNo.error.required"))
      }
    }

    "must redirect to Journey Recovery for a GET when subcontractor name is missing (userAnswers present)" in {

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, subAddressYesNoRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST when subcontractor name is missing (userAnswers present)" in {

      val mockSessionRepository = mock[SessionRepository]
      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(bind[SessionRepository].toInstance(mockSessionRepository))
          .build()

      running(application) {
        val request =
          FakeRequest(POST, subAddressYesNoRoute)
            .withFormUrlEncodedBody("value" -> "true")

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
