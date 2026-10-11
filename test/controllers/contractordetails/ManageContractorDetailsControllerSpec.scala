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
import models.contractordetails.ContractorDetailsValidationTarget
import models.{Scheme, UserAnswers}
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.{any, eq as eqTo}
import org.mockito.Mockito.{atLeastOnce, verify, when}
import org.scalatestplus.mockito.MockitoSugar
import pages.contractordetails.{ContractorDetailsValidationTargetPage, ContractorSchemePage}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import queries.CisIdQuery
import repositories.SessionRepository
import services.{CisManageService, ContractorDetailsService}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future
import scala.jdk.CollectionConverters.*

class ManageContractorDetailsControllerSpec extends SpecBase with MockitoSugar {

  private val mockContractorDetailsService =
    mock[ContractorDetailsService]

  private val mockSessionRepository =
    mock[SessionRepository]

  private val schemeWithUtr = Scheme(
    schemeId = 1,
    instanceId = "cisId",
    accountsOfficeReference = "123 PA 87654321",
    taxOfficeNumber = "123",
    taxOfficeReference = "45678",
    utr = Some("1234567890"),
    name = Some("ABC Ltd"),
    emailAddress = Some("test@test.com")
  )

  private val schemeWithoutUtr =
    schemeWithUtr.copy(
      utr = None
    )

  private def applicationWith(
    userAnswers: UserAnswers,
    mockContractorDetailsService: ContractorDetailsService,
    mockSessionRepository: SessionRepository
  ): GuiceApplicationBuilder = {

    val mockCisManageService =
      mock[CisManageService]

    when(
      mockCisManageService.ensureCisIdInUserAnswers(
        any[UserAnswers]
      )(any[HeaderCarrier])
    ).thenReturn(
      Future.successful(userAnswers)
    )

    applicationBuilder(Some(emptyUserAnswers))
      .overrides(
        bind[ContractorDetailsService]
          .toInstance(mockContractorDetailsService),
        bind[CisManageService]
          .toInstance(mockCisManageService),
        bind[SessionRepository]
          .toInstance(mockSessionRepository)
      )
  }

  "ManageContractorDetailsController" - {

    "must redirect to ContractorDetailsCheckAnswers when utr exists" in {

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, "cisId")
          .success
          .value

      when(
        mockContractorDetailsService.getScheme(eqTo("cisId"))(any())
      ).thenReturn(
        Future.successful(schemeWithUtr)
      )

      when(
        mockSessionRepository.set(any())
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController.onPageLoad().url
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.contractordetails.routes.ContractorDetailsCheckAnswersController
            .onPageLoad()
            .url
      }
    }

    "must redirect to ContractorDetailsController when utr does not exist" in {

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, "cisId")
          .success
          .value

      when(
        mockContractorDetailsService.getScheme(eqTo("cisId"))(any())
      ).thenReturn(
        Future.successful(schemeWithoutUtr)
      )

      when(
        mockSessionRepository.set(any())
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController.onPageLoad().url
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.contractordetails.routes.ContractorDetailsController
            .onPageLoad()
            .url
      }
    }

    "must store the scheme in user answers before redirecting to check answers" in {

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, "cisId")
          .success
          .value

      when(
        mockContractorDetailsService.getScheme(any[String])(any())
      ).thenReturn(
        Future.successful(schemeWithUtr)
      )

      when(
        mockSessionRepository.set(any[UserAnswers])
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController.onPageLoad().url
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        val userAnswersCaptor =
          ArgumentCaptor.forClass(classOf[UserAnswers])

        verify(mockSessionRepository, atLeastOnce())
          .set(userAnswersCaptor.capture())

        val savedAnswers =
          userAnswersCaptor.getAllValues.asScala.last

        savedAnswers.get(ContractorSchemePage) mustBe Some(schemeWithUtr)
      }
    }

    "must redirect to JourneyRecovery when getScheme fails" in {

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, "cisId")
          .success
          .value

      when(
        mockContractorDetailsService.getScheme(eqTo("cisId"))(any())
      ).thenReturn(
        Future.failed(new RuntimeException("boom"))
      )

      val application =
        applicationWith(
          userAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController.onPageLoad().url
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

    "must redirect to JourneyRecovery when CisIdQuery is missing" in {

      val application =
        applicationWith(
          emptyUserAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController.onPageLoad().url
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

    "must redirect to ContractorDetailsController when utr is blank" in {

      val schemeWithBlankUtr =
        schemeWithUtr.copy(
          utr = Some("   ")
        )

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, "cisId")
          .success
          .value

      when(
        mockContractorDetailsService.getScheme(eqTo("cisId"))(any())
      ).thenReturn(
        Future.successful(schemeWithBlankUtr)
      )

      when(
        mockSessionRepository.set(any())
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController.onPageLoad().url
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.contractordetails.routes.ContractorDetailsController
            .onPageLoad()
            .url
      }
    }

    "must preserve target when redirecting to ContractorDetailsCheckAnswers" in {

      val userAnswers =
        emptyUserAnswers
          .set(CisIdQuery, "cisId")
          .success
          .value
          .set(
            ContractorDetailsValidationTargetPage,
            ContractorDetailsValidationTarget.FileNilReturn
          )
          .success
          .value

      when(
        mockContractorDetailsService.getScheme(eqTo("cisId"))(any())
      ).thenReturn(
        Future.successful(schemeWithUtr)
      )

      when(
        mockSessionRepository.set(any())
      ).thenReturn(
        Future.successful(true)
      )

      val application =
        applicationWith(
          userAnswers,
          mockContractorDetailsService,
          mockSessionRepository
        ).build()

      running(application) {

        val target = "manageYourCisReturn"

        val request =
          FakeRequest(
            GET,
            routes.ManageContractorDetailsController
              .onPageLoad(Some(target))
              .url
          )

        val result =
          route(application, request).value

        status(result) mustBe SEE_OTHER

        redirectLocation(result).value mustBe
          controllers.contractordetails.routes.ContractorDetailsCheckAnswersController
            .onPageLoad(Some(target))
            .url

        val userAnswersCaptor =
          ArgumentCaptor.forClass(classOf[UserAnswers])

        verify(mockSessionRepository, atLeastOnce())
          .set(userAnswersCaptor.capture())

        val savedAnswers =
          userAnswersCaptor.getAllValues.asScala.last

        savedAnswers.get(ContractorDetailsValidationTargetPage) mustBe None

        savedAnswers.get(ContractorSchemePage) mustBe Some(schemeWithUtr)
      }
    }
  }
}
