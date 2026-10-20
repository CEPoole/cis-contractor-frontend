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
import play.api.mvc.{Action, AnyContent, BodyParsers, Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.retrieve.~
import models.requests.IdentifierRequest

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}

class AgentIdentifierActionSpec extends SpecBase {

  private val mockAuthConnector: AuthConnector = mock[AuthConnector]
  private val application                      = applicationBuilder(userAnswers = None)
    .overrides(bind[AuthConnector].toInstance(mockAuthConnector))
    .build()
  private val bodyParsers                      = application.injector.instanceOf[BodyParsers.Default]
  private val appConfig                        = application.injector.instanceOf[FrontendAppConfig]
  private val emptyEnrolments                  = Enrolments(Set.empty)
  private val id: String                       = UUID.randomUUID().toString

  private val mockPolicyResolver: ClientListCheckPolicyResolver = mock[ClientListCheckPolicyResolver]
  private val mockClientListStatusGuard: ClientListStatusGuard  = mock[ClientListStatusGuard]
  private val mockHasClientGuard: HasClientGuard                = mock[HasClientGuard]
  private val clientListCheckEnforcer: ClientListCheckEnforcer  =
    new ClientListCheckEnforcer(
      mockPolicyResolver,
      mockClientListStatusGuard,
      mockHasClientGuard
    ) {
      override def apply[A](request: IdentifierRequest[A])(
        block: IdentifierRequest[A] => Future[Result]
      ): Future[Result] =
        block(request)
    }

  private val agentCode: String = "123456789"

  type RetrievalsType = Option[String] ~ Enrolments ~ Option[AffinityGroup] ~ Option[CredentialRole] ~ Option[String]

  class Harness(authAction: IdentifierAction) {
    def onPageLoad(): Action[AnyContent] = authAction(_ => Results.Ok)
  }

  "Agent Identifier Action" - {

    "when the user hasn't logged in" - {
      "must redirect the user to log in " in {
        running(application) {
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new MissingBearerToken),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
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
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new BearerTokenExpired),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
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
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new InsufficientEnrolments),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
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
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new InsufficientConfidenceLevel),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
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
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new UnsupportedAuthProvider),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
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
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new UnsupportedAffinityGroup),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
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
          val authAction = new AgentIdentifierAction(
            new FakeFailingAuthConnector(new UnsupportedCredentialRole),
            appConfig,
            bodyParsers,
            clientListCheckEnforcer
          )
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result) mustBe Some(routes.UnauthorisedController.onPageLoad().url)
        }
      }
    }

    "the user is logged in as an  agent" - {
      "and is allowed into the service" - {
        "must succeed" - {
          "when the user has a IR-PAYE-AGENT enrolment with the correct activated identifiers" in {
            val enrolments = Enrolments(
              Set(
                Enrolment(
                  "IR-PAYE-AGENT",
                  Seq(
                    EnrolmentIdentifier("IRAgentReference", "123456")
                  ),
                  "activated",
                  None
                )
              )
            )
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(
                Future.successful(Some(id) ~ enrolments ~ Some(Agent) ~ None ~ Some(agentCode))
              )
            running(application) {
              val authAction =
                new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())

              status(result) mustBe OK
            }
          }
        }
      }
      "and is not allowed into the service" - {
        "when there is no IR-PAYE-AGENT enrolment" - {
          "must redirect the user to unauthorised agent affinity screen" in {
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(Future.successful(Some(id) ~ emptyEnrolments ~ Some(Agent) ~ None ~ Some(agentCode)))
            running(application) {
              val authAction =
                new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())

              status(result) mustBe SEE_OTHER
              redirectLocation(result).value mustBe controllers.routes.UnauthorisedAgentAffinityController
                .onPageLoad()
                .url
            }
          }
        }
        "when there is an inactive IR-PAYE-AGENT enrolment" - {
          "must redirect to unauthorised agent affinity screen" in {
            val enrolments = Enrolments(
              Set(
                Enrolment(
                  "IR-PAYE-AGENT",
                  Seq(
                    EnrolmentIdentifier("IRAgentReference", "123456")
                  ),
                  "inactivated",
                  None
                )
              )
            )
            when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
              .thenReturn(
                Future.successful(Some(id) ~ enrolments ~ Some(Agent) ~ None ~ Some(agentCode))
              )
            running(application) {
              val authAction =
                new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
              val controller = new Harness(authAction)
              val result     = controller.onPageLoad()(FakeRequest())
              status(result) mustBe SEE_OTHER
              redirectLocation(
                result
              ).value mustBe controllers.routes.UnauthorisedAgentAffinityController
                .onPageLoad()
                .url
            }
          }
        }
      }
    }

    "the user is logged in as an individual" - {
      "fail and redirect to unauthorised individual affinity screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(
            Future.successful(Some(id) ~ emptyEnrolments ~ Some(Individual) ~ Some(Assistant) ~ None)
          )
        running(application) {
          val authAction = new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
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
      "ignore the credential role and redirect to unauthorised screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(
            Future.successful(Some(id) ~ emptyEnrolments ~ Some(Organisation) ~ Some(Assistant) ~ None)
          )
        running(application) {
          val authAction = new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe controllers.routes.UnauthorisedController
            .onPageLoad()
            .url
        }
      }
    }

    "the user is logged in as an organisation User" - {
      "must redirect to unauthorised screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(
            Future.successful(Some(id) ~ emptyEnrolments ~ Some(Organisation) ~ Some(User) ~ None)
          )
        running(application) {
          val authAction = new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())
          status(result) mustBe SEE_OTHER
          redirectLocation(
            result
          ).value mustBe controllers.routes.UnauthorisedController
            .onPageLoad()
            .url
        }
      }
    }

    "Unable to retrieve internal id or affinity group" - {
      "fail and redirect to Unauthorised screen" in {
        when(mockAuthConnector.authorise[RetrievalsType](any(), any())(any(), any()))
          .thenReturn(Future.successful(None ~ emptyEnrolments ~ None ~ None ~ None))
        running(application) {
          val authAction = new AgentIdentifierAction(mockAuthConnector, appConfig, bodyParsers, clientListCheckEnforcer)
          val controller = new Harness(authAction)
          val result     = controller.onPageLoad()(FakeRequest())

          status(result) mustBe SEE_OTHER
          redirectLocation(result).value mustBe controllers.routes.UnauthorisedController.onPageLoad().url
        }
      }
    }

  }
}
