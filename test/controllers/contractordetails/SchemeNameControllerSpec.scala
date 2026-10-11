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

import base.SpecBase
import controllers.routes
import forms.contractordetails.SchemeNameFormProvider
import models.{NormalMode, UserAnswers}
import navigation.{FakeNavigator, Navigator}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.contractordetails.{AddSchemeNameYesNoPage, SchemeNamePage}
import play.api.data.Form
import play.api.inject.bind
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import repositories.SessionRepository
import views.html.contractordetails.SchemeNameView

import scala.concurrent.Future

class SchemeNameControllerSpec extends SpecBase with MockitoSugar {

  def onwardRoute: Call = Call("GET", "/foo")

  val formProvider       = new SchemeNameFormProvider()
  val form: Form[String] = formProvider()

  lazy val schemeNameRoute: String =
    controllers.contractordetails.routes.SchemeNameController
      .onPageLoad(NormalMode)
      .url

  val userAnswersForSchemeNamePage: UserAnswers =
    emptyUserAnswers
      .set(AddSchemeNameYesNoPage, true)
      .success
      .value

  "SchemeName Controller" - {

    "must return OK and the correct view for a GET when AddSchemeNameYesNoPage is true" in {

      val application =
        applicationBuilder(userAnswers = Some(userAnswersForSchemeNamePage)).build()

      running(application) {
        val request = FakeRequest(GET, schemeNameRoute)

        val result = route(application, request).value

        val view =
          application.injector.instanceOf[SchemeNameView]

        status(result) mustEqual OK
        contentAsString(result) mustEqual
          view(form, NormalMode)(request, messages(application)).toString
      }
    }

    "must populate the view correctly on a GET when the scheme name has previously been answered" in {

      val userAnswers =
        userAnswersForSchemeNamePage
          .set(SchemeNamePage, "answer")
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, schemeNameRoute)

        val view =
          application.injector.instanceOf[SchemeNameView]

        val result = route(application, request).value

        status(result) mustEqual OK
        contentAsString(result) mustEqual
          view(form.fill("answer"), NormalMode)(
            request,
            messages(application)
          ).toString
      }
    }

    "must redirect to AddSchemeNameYesNo page on a GET when AddSchemeNameYesNoPage has not been answered" in {

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, schemeNameRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.contractordetails.routes.AddSchemeNameYesNoController
            .onPageLoad(NormalMode)
            .url
      }
    }

    "must redirect to AddSchemeNameYesNo page on a GET when AddSchemeNameYesNoPage is false" in {

      val userAnswers =
        emptyUserAnswers
          .set(AddSchemeNameYesNoPage, false)
          .success
          .value

      val application =
        applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {
        val request = FakeRequest(GET, schemeNameRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          controllers.contractordetails.routes.AddSchemeNameYesNoController
            .onPageLoad(NormalMode)
            .url
      }
    }

    "must redirect to the next page when valid data is submitted" in {

      val mockSessionRepository = mock[SessionRepository]

      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(userAnswersForSchemeNamePage))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {
        val request =
          FakeRequest(POST, schemeNameRoute)
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "must return a Bad Request and errors when invalid data is submitted" in {

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {
        val request =
          FakeRequest(POST, schemeNameRoute)
            .withFormUrlEncodedBody(("value", ""))

        val boundForm = form.bind(Map("value" -> ""))

        val view =
          application.injector.instanceOf[SchemeNameView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST
        contentAsString(result) mustEqual
          view(boundForm, NormalMode)(request, messages(application)).toString
      }
    }

    "must redirect to Journey Recovery for a GET if no existing data is found" in {

      val application =
        applicationBuilder(userAnswers = None).build()

      running(application) {
        val request = FakeRequest(GET, schemeNameRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          routes.JourneyRecoveryController.onPageLoad().url
      }
    }

    "must redirect to Journey Recovery for a POST if no existing data is found" in {

      val application =
        applicationBuilder(userAnswers = None).build()

      running(application) {
        val request =
          FakeRequest(POST, schemeNameRoute)
            .withFormUrlEncodedBody(("value", "answer"))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual
          routes.JourneyRecoveryController.onPageLoad().url
      }
    }
  }
}
