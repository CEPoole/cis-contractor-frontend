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

package controllers.actions

import base.SpecBase
import config.FrontendAppConfig
import controllers.actions.TestAuthRetrievals.Ops
import controllers.routes
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar.mock
import play.api.inject.bind
import play.api.mvc.{Action, AnyContent, BodyParsers, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.retrieve.~

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class ContractorIdentificationActionSpec extends SpecBase {

  private val mockAuthConnector: AuthConnector = mock[AuthConnector]
  private val application                      = applicationBuilder(userAnswers = None)
    .overrides(bind[AuthConnector].toInstance(mockAuthConnector))
    .build()
  private val bodyParsers                      = application.injector.instanceOf[BodyParsers.Default]
  private val appConfig                        = application.injector.instanceOf[FrontendAppConfig]
  private val emptyEnrolments                  = Enrolments(Set.empty)
  private val id: String                       = UUID.randomUUID().toString

  type RetrievalsType = Option[String] ~ Enrolments ~ Option[AffinityGroup] ~ Option[CredentialRole]

  class Harness(authAction: IdentifierAction) {
    def onPageLoad(): Action[AnyContent] = authAction(_ => Results.Ok)
  }

  "Contractor Identification Action" - {

    "when the user hasn't logged in" - {
      "must redirect the user to log in " in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new MissingBearerToken),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value must startWith(appConfig.loginUrl)
        }
      }
    }

    "the user's session has expired" - {
      "must redirect the user to log in " in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new BearerTokenExpired),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value must startWith(appConfig.loginUrl)
        }
      }
    }

    "the user doesn't have sufficient enrolments" - {
      "must redirect the user to the unauthorised page" in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new InsufficientEnrolments),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe routes.UnauthorisedController.onPageLoad().url
        }
      }
    }

    "the user doesn't have sufficient confidence level" - {
      "must redirect the user to the unauthorised page" in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new InsufficientConfidenceLevel),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe routes.UnauthorisedController.onPageLoad().url
        }
      }
    }

    "the user used an unaccepted auth provider" - {
      "must redirect the user to the unauthorised page" in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new UnsupportedAuthProvider),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe routes.UnauthorisedController.onPageLoad().url
        }
      }
    }

    "the user has an unsupported affinity group" - {
      "must redirect the user to the unauthorised page" in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new UnsupportedAffinityGroup),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe Some(routes.UnauthorisedController.onPageLoad().url)
        }
      }
    }

    "the user has an unsupported credential role" - {
      "must redirect the user to the unauthorised page" in {
        running(application) {
          val authAction = new ContractorIdentifierAction(
            new FakeFailingAuthConnector(new UnsupportedCredentialRole),
            appConfig,
            bodyParsers
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe Some(routes.UnauthorisedController.onPageLoad().url)
        }
      }
    }

    "the user is logged in as an agent" - {
      "fail and redirect to unauthorised screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(Future.successful(Some(id) ~ emptyEnrolments ~ Some(Agent) ~ None))
        running(application) {
          val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe controllers.routes.UnauthorisedController
            .onPageLoad()
            .url
        }
      }
    }

    "the user is logged in as an individual" - {
      "fail and redirect to unauthorised individual affinity screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(
            Future.successful(Some(id) ~ emptyEnrolments ~ Some(Individual) ~ Some(Assistant))
          )
        running(application) {
          val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(
            result
          ).value mustBe controllers.routes.UnauthorisedIndividualAffinityController.onPageLoad().url
        }
      }
    }

    "the user is logged in as an organisation assistant" - {
      "ignore the credential role and redirect to unauthorised organisation affinity screen when there is no CIS enrolment" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(
            Future.successful(Some(id) ~ emptyEnrolments ~ Some(Organisation) ~ Some(Assistant))
          )
        running(application) {
          val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe controllers.routes.UnauthorisedOrganisationAffinityController
            .onPageLoad()
            .url
        }
      }
    }

    "the user is logged in as an organisation User" - {
      "and is allowed into the service" - {
        "must succeed" - {
          "when the user has a HMRC-CIS-ORG enrolment with the correct activated identifiers" in {
            val enrolments = Enrolments(
              Set(
                Enrolment(
                  "HMRC-CIS-ORG",
                  Seq(
                    EnrolmentIdentifier("TaxOfficeNumber", "754"),
                    EnrolmentIdentifier("TaxOfficeReference", "EZ00047")
                  ),
                  "activated",
                  None
                )
              )
            )
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(
                Future.successful(Some(id) ~ enrolments ~ Some(Organisation) ~ Some(User))
              )
            running(application) {
              val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())

              status(result) mustBe OK
            }
          }

          "when an organisation assistant has a HMRC-CIS-ORG enrolment with the correct activated identifiers" in {
            val enrolments = Enrolments(
              Set(
                Enrolment(
                  "HMRC-CIS-ORG",
                  Seq(
                    EnrolmentIdentifier("TaxOfficeNumber", "754"),
                    EnrolmentIdentifier("TaxOfficeReference", "EZ00047")
                  ),
                  "activated",
                  None
                )
              )
            )
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(
                Future.successful(Some(id) ~ enrolments ~ Some(Organisation) ~ Some(Assistant))
              )
            running(application) {
              val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())

              status(result) mustBe OK
            }
          }
        }
      }

      "and is not allowed into the service" - {
        "when there is no HMRC-CIS-ORG enrolment" - {
          "must redirect to unauthorised organisation affinity screen" in {
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(
                Future.successful(Some(id) ~ emptyEnrolments ~ Some(Organisation) ~ Some(User))
              )
            running(application) {
              val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())

              status(result) mustBe SEE_OTHER
              redirectLocation(
                result
              ).value mustBe controllers.routes.UnauthorisedOrganisationAffinityController
                .onPageLoad()
                .url
            }
          }
        }
        "when there is an inactive HMRC-CIS-ORG enrolment" - {
          "must redirect to unauthorised organisation affinity screen" in {
            val enrolments = Enrolments(
              Set(
                Enrolment(
                  "HMRC-CIS-ORG",
                  Seq(
                    EnrolmentIdentifier("TaxOfficeNumber", "754"),
                    EnrolmentIdentifier("TaxOfficeReference", "EZ00047")
                  ),
                  "inactivated",
                  None
                )
              )
            )
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(
                Future.successful(Some(id) ~ enrolments ~ Some(Organisation) ~ Some(User))
              )
            running(application) {
              val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())
              status(result) mustBe SEE_OTHER
              redirectLocation(
                result
              ).value mustBe controllers.routes.UnauthorisedOrganisationAffinityController
                .onPageLoad()
                .url
            }
          }
        }
      }
    }

    "Unable to retrieve internal id or affinity group" - {
      "fail and redirect to Unauthorised screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(Future.successful(None ~ emptyEnrolments ~ None ~ None))
        running(application) {
          val authAction = new ContractorIdentifierAction(mockAuthConnector, appConfig, bodyParsers)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe controllers.routes.UnauthorisedController.onPageLoad().url
        }
      }
    }

  }
}
