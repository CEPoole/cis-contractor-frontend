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

import com.google.inject.Inject
import config.FrontendAppConfig
import controllers.routes
import models.requests.IdentifierRequest
import play.api.Logging
import play.api.mvc.*
import play.api.mvc.Results.*
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.AffinityGroup.{Agent, Individual, Organisation}
import uk.gov.hmrc.auth.core.AuthProvider.GovernmentGateway
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import utils.AuthUtils.hasCisOrgEnrolment

import javax.inject.Named
import scala.concurrent.{ExecutionContext, Future}

@Named("ContractorIdentifier")
class ContractorIdentifierAction @Inject() (
  override val authConnector: AuthConnector,
  config: FrontendAppConfig,
  val parser: BodyParsers.Default
)(implicit val executionContext: ExecutionContext)
    extends IdentifierAction
    with AuthorisedFunctions
    with Logging {

  override def invokeBlock[A](request: Request[A], block: IdentifierRequest[A] => Future[Result]): Future[Result] = {
    implicit val hc: HeaderCarrier  = HeaderCarrierConverter.fromRequestAndSession(request, request.session)
    val defaultPredicate: Predicate = AuthProviders(GovernmentGateway)
    authorised(defaultPredicate)
      .retrieve(
        Retrievals.internalId and Retrievals.allEnrolments
          and Retrievals.affinityGroup and Retrievals.credentialRole
      ) {
        case Some(internalId) ~ Enrolments(enrolments) ~ Some(Organisation) ~ _ =>
          hasCisOrgEnrolment(enrolments)
            .map { employerReference =>
              block(IdentifierRequest(request, internalId, Some(employerReference), None))
            }
            .getOrElse(
              Future.successful(
                Redirect(controllers.routes.UnauthorisedOrganisationAffinityController.onPageLoad())
              )
            )
        case Some(_) ~ _ ~ Some(Individual) ~ _                                 =>
          logger.info("ContractorIdentifierAction - Individual login attempt")
          Future.successful(
            Redirect(controllers.routes.UnauthorisedIndividualAffinityController.onPageLoad())
          )
        case Some(internalId) ~ Enrolments(enrolments) ~ Some(Agent) ~ _        =>
          logger.warn("ContractorIdentifierAction - Agent login attempt")
          Future.successful(
            Redirect(controllers.routes.UnauthorisedController.onPageLoad())
          )
        case _                                                                  =>
          logger.warn("ContractorIdentifierAction - Unable to retrieve internal id or affinity group")
          Future.successful(Redirect(routes.UnauthorisedController.onPageLoad()))
      } recover {
      case _: NoActiveSession        =>
        Redirect(config.loginUrl, Map("continue" -> Seq(config.loginContinueUrl)))
      case _: AuthorisationException =>
        logger.warn("ContractorIdentifierAction - AuthorisationException")
        Redirect(routes.UnauthorisedController.onPageLoad())
    }
  }

}
