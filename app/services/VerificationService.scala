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

import connectors.ConstructionIndustrySchemeConnector
import models.agent.AgentClientData
import models.requests.*
import models.response.*
import models.verify.*
import models.verify.VerificationBatchStatus.*
import models.{EmployerReference, Subcontractor, UserAnswers}
import pages.verify.*
import play.api.Logging
import play.api.i18n.Messages
import play.api.libs.json.*
import play.api.mvc.AnyContent
import queries.CisIdQuery
import repositories.SessionRepository
import uk.gov.hmrc.http.HeaderCarrier
import utils.SubmissionUtils

import java.time.{Clock, LocalDateTime, ZoneId}
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class VerificationService @Inject() (
  cisConnector: ConstructionIndustrySchemeConnector,
  cisManageService: CisManageService,
  chrisVerificationRequestBuilder: ChrisVerificationRequestBuilder,
  sessionRepository: SessionRepository,
  submissionUtils: SubmissionUtils,
  clock: Clock
)(implicit ec: ExecutionContext)
    extends Logging {

  private val ukZone = ZoneId.of("Europe/London")

  def refreshNewestVerificationBatch(userAnswers: UserAnswers)(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      instanceId <- userAnswers
                      .get(CisIdQuery)
                      .map(Future.successful)
                      .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found in session data")))

      response  <- cisConnector.getNewestVerificationBatch(instanceId)
      unverified = unverifiedSubcontractors(response.subcontractors)

      updated <- Future.fromTry(
                   userAnswers
                     .set(NewestVerificationBatchResponsePage, response)
                     .flatMap(_.set(UnverifiedSubcontractorsPage, unverified))
                     .flatMap(cleanupSelectionsNoLongerInNewestBatch(response))
                 )

      _ <- sessionRepository.set(updated)
    } yield updated

  private def cleanupSelectionsNoLongerInNewestBatch(
    response: models.response.GetNewestVerificationBatchResponse
  )(userAnswers: UserAnswers): Try[UserAnswers] = {
    val newestSubcontractorIds =
      response.subcontractors.map(_.subcontractorId.toString).toSet

    val withSelectedSubcontractors =
      userAnswers.get(SelectSubcontractorPage) match {
        case Some(selected) =>
          userAnswers.set(
            SelectSubcontractorPage,
            selected.filter(subcontractor => newestSubcontractorIds.contains(subcontractor.id))
          )

        case None =>
          scala.util.Success(userAnswers)
      }

    withSelectedSubcontractors.flatMap { updatedAnswers =>
      updatedAnswers.get(SelectSubcontractorsToReverifyPage) match {
        case Some(selected) =>
          updatedAnswers.set(
            SelectSubcontractorsToReverifyPage,
            selected.filter(subcontractor => newestSubcontractorIds.contains(subcontractor.id))
          )

        case None =>
          scala.util.Success(updatedAnswers)
      }
    }
  }

  def refreshSubmittedVerificationRequest(userAnswers: UserAnswers)(implicit hc: HeaderCarrier): Future[UserAnswers] =
    refreshNewestVerificationBatch(userAnswers).flatMap { updatedAnswers =>
      val status =
        updatedAnswers
          .get(NewestVerificationBatchResponsePage)
          .flatMap(_.submission)
          .flatMap(_.status)

      if (status.exists(isSubmittedStatus)) {
        Future.successful(updatedAnswers)
      } else {
        Future.failed(
          new IllegalStateException(
            s"Submitted verification request page cannot be accessed for submission status: ${status.getOrElse("missing")}"
          )
        )
      }
    }

  def getCurrentVerificationBatch(userAnswers: UserAnswers)(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      instanceId <- userAnswers
                      .get(CisIdQuery)
                      .map(Future.successful)
                      .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found in session data")))

      response <- cisConnector.getCurrentVerificationBatch(instanceId)
      updated  <- Future.fromTry(userAnswers.set(CurrentVerificationBatchResponsePage, response))
      _        <- sessionRepository.set(updated)
    } yield updated

  def getLastSubmittedVerificationBatch(userAnswers: UserAnswers)(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      instanceId <- userAnswers
                      .get(CisIdQuery)
                      .map(Future.successful)
                      .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found in session data")))

      response <- cisConnector.getLastSubmittedVerificationBatch(instanceId)
      updated  <- Future.fromTry(userAnswers.set(LastSubmittedVerificationBatchResponsePage, response))
      _        <- sessionRepository.set(updated)
    } yield updated

  def latestBatchCanBeModified(userAnswers: UserAnswers): Boolean =
    latestBatchStatus(userAnswers).exists {
      case Started | Validated => true
      case _                   => false
    }

  private def latestBatchCanBeRecreated(userAnswers: UserAnswers): Boolean =
    latestBatchStatus(userAnswers).exists {
      case Submitted | SubmittedNoReceipt | DepartmentalError | FatalError => true
      case _                                                               => false
    }

  private def latestBatchStatus(userAnswers: UserAnswers): Option[VerificationBatchStatus] =
    userAnswers
      .get(NewestVerificationBatchResponsePage)
      .flatMap(_.verificationBatch)
      .flatMap(_.status)
      .flatMap(VerificationBatchStatus.from)

  def createVerificationBatchAndVerifications(
    userAnswers: UserAnswers,
    selectedSubcontractorIds: Seq[Long],
    actionIndicator: Option[String] = None
  )(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      instanceId <- userAnswers
                      .get(CisIdQuery)
                      .map(Future.successful)
                      .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found in session data")))

      _ <- if (selectedSubcontractorIds.nonEmpty) Future.successful(())
           else Future.failed(new RuntimeException("No subcontractors selected"))

      idToRef <- subcontractorResourceRefs(userAnswers)

      verificationResourceRefs <- Future.fromTry {
                                    scala.util.Try {
                                      selectedSubcontractorIds.distinct.map { id =>
                                        idToRef.getOrElse(
                                          id,
                                          throw new RuntimeException(
                                            s"Missing subbieResourceRef for subcontractorId=$id in current verification batch"
                                          )
                                        )
                                      }
                                    }
                                  }

      _ <- cisConnector.createVerificationBatchAndVerifications(
             CreateVerificationBatchAndVerificationsRequest(
               instanceId = instanceId,
               verificationResourceReferences = verificationResourceRefs,
               actionIndicator = actionIndicator
             )
           )

      afterCurrent <- getCurrentVerificationBatch(userAnswers)
      afterNewest  <- refreshNewestVerificationBatch(afterCurrent)
      _            <- sessionRepository.set(afterNewest)
    } yield afterNewest

  private def subcontractorResourceRefs(userAnswers: UserAnswers): Future[Map[Long, Long]] =
    userAnswers
      .get(NewestVerificationBatchResponsePage)
      .map { newest =>
        Future.successful(
          newest.subcontractors
            .flatMap(s => s.subbieResourceRef.map(ref => s.subcontractorId -> ref))
            .toMap
        )
      }
      .orElse {
        userAnswers
          .get(CurrentVerificationBatchResponsePage)
          .map { current =>
            Future.successful(
              current.subcontractors
                .flatMap(s => s.subbieResourceRef.map(ref => s.subcontractorId -> ref))
                .toMap
            )
          }
      }
      .getOrElse(
        Future.failed(
          new RuntimeException(
            "Neither CurrentVerificationBatchResponsePage nor NewestVerificationBatchResponsePage found in session data"
          )
        )
      )

  private def unverifiedSubcontractors(
    subcontractors: Seq[Subcontractor]
  ): Seq[Subcontractor] =
    subcontractors.filter(isUnverified)

  private def isUnverified(sub: Subcontractor): Boolean =
    !sub.verified.contains("Y")

  private def isSubmittedStatus(status: String): Boolean =
    SubmissionStatus.fromString(status) match {
      case SubmissionStatus.SUBMITTED | SubmissionStatus.SUBMITTED_NO_RECEIPT => true
      case _                                                                  => false
    }

  def modifyVerificationBatchAndVerifications(
    userAnswers: UserAnswers,
    request: ModifyVerificationsRequest
  )(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      _            <- cisConnector.modifyVerificationBatch(request)
      afterCurrent <- getCurrentVerificationBatch(userAnswers)
      afterNewest  <- refreshNewestVerificationBatch(afterCurrent)
      _            <- sessionRepository.set(afterNewest)
    } yield afterNewest

  def deleteVerification(
    userAnswers: UserAnswers,
    verificationResourceRef: Long
  )(implicit hc: HeaderCarrier): Future[DeleteVerificationResponse] =
    for {
      instanceId <- userAnswers
                      .get(CisIdQuery)
                      .map(Future.successful)
                      .getOrElse(Future.failed(new RuntimeException("InstanceIdQuery not found in session data")))

      response <- cisConnector.deleteVerification(
                    DeleteVerificationRequest(
                      instanceId = instanceId,
                      verificationResourceRef = verificationResourceRef
                    )
                  )

      afterCurrent <- getCurrentVerificationBatch(userAnswers)
      afterNewest  <- refreshNewestVerificationBatch(afterCurrent)
      updated      <- Future.fromTry(withVerificationBatchReadiness(afterNewest))
      _            <- sessionRepository.set(updated)
    } yield response

  private def withVerificationBatchReadiness(userAnswers: UserAnswers): Try[UserAnswers] =
    userAnswers
      .get(CurrentVerificationBatchResponsePage)
      .map { batch =>
        val remainingVerifications =
          batch.verifications.flatMap { verification =>
            verification.subcontractorId.flatMap { subcontractorId =>
              batch.subcontractors
                .find(_.subcontractorId == subcontractorId)
                .map(subcontractor => (subcontractor, verification))
            }
          }

        val batchReady =
          remainingVerifications.forall { case (subcontractor, verification) =>
            VerificationBatchReadiness.isSubcontractorReady(subcontractor, Some(verification))
          }

        userAnswers.set(VerificationBatchReadinessPage, batchReady)
      }
      .getOrElse(userAnswers.remove(VerificationBatchReadinessPage))

  def createSubmitAndPersistVerificationSubmission(implicit
    request: DataRequest[AnyContent],
    hc: HeaderCarrier,
    messages: Messages
  ): Future[ChrisSubmissionResponse] =
    for {
      latestUa      <- getCurrentVerificationBatch(request.userAnswers)
      createRequest <- buildCreateSubmissionRequest(latestUa)
      submissionId  <- createSubmissionForVerification(createRequest).map(_.submissionId)
      response      <- submitVerificationToChris(submissionId, latestUa)
      updatedUa     <- saveVerificationSubmissionDetailsToSession(latestUa, response)
    } yield response

  def pollStatusAndPersist(
    ua: UserAnswers,
    submissionDetails: VerificationSubmissionDetails
  )(implicit hc: HeaderCarrier): Future[ChrisPollResponse] =
    for {
      pollUrl          <- required(submissionDetails.pollUrl, "Poll URL missing in submission details")
      response         <- cisConnector.getSubmissionStatus(pollUrl, submissionDetails.submissionId)
      effectiveResponse = response.copy(status = effectivePollStatus(response, submissionDetails.submittedAt))
      updatedDetails    = VerificationSubmissionDetailsBuilder.updateFromPollResponse(submissionDetails, effectiveResponse)
      updatedUa        <- saveVerificationPollDetailsToSession(ua, updatedDetails)
    } yield effectiveResponse

  // A poll is only due once the poll interval has elapsed since the last message from ChRIS:
  // the acknowledgement time (submittedAt) for the first poll, then each poll response's
  // timestamp thereafter. This delays the first poll and throttles subsequent polls rather
  // than firing on every page refresh.
  def isPollDue(submissionDetails: VerificationSubmissionDetails, pollInterval: Int): Boolean = {
    val lastMessageTime = submissionDetails.lastMessageDate.getOrElse(submissionDetails.submittedAt)
    !LocalDateTime.now(clock.withZone(ukZone)).isBefore(lastMessageTime.plusSeconds(pollInterval))
  }

  // F18: while ChRIS is still processing (or its poll endpoint is erroring) the backend keeps
  // reporting ACCEPTED/PENDING; once the polling window is exhausted the user must be routed
  // to "send error" (SM-06) if polls were failing with timeOut errors, or "in progress" otherwise.
  private def effectivePollStatus(
    response: ChrisPollResponse,
    submittedAt: LocalDateTime
  ): SubmissionStatus =
    response.status match {
      case SubmissionStatus.ACCEPTED | SubmissionStatus.PENDING if pollingWindowExhausted(submittedAt) =>
        if (isTimeoutError(response)) SubmissionStatus.SEND_ERROR else SubmissionStatus.TIMED_OUT
      case other                                                                                       =>
        other
    }

  private def pollingWindowExhausted(submittedAt: LocalDateTime): Boolean =
    !LocalDateTime.now(clock.withZone(ukZone)).isBefore(submissionUtils.calculateTimeoutDateTime(submittedAt))

  private def isTimeoutError(response: ChrisPollResponse): Boolean = {
    val hasTimeoutGovTalkError = response.error.exists { error =>
      (error \ "type").asOpt[String].exists(_.equalsIgnoreCase("timeOut"))
    }
    val hasNoResponseStatus    = response.govTalkErrorStatus.exists {
      case GovTalkErrorStatus.ServerError(_) | GovTalkErrorStatus.NoResponse => true
      case _                                                                 => false
    }
    hasTimeoutGovTalkError || hasNoResponseStatus
  }

  private def createSubmissionForVerification(
    request: CreateSubmissionForVerificationRequest
  )(implicit hc: HeaderCarrier): Future[CreateSubmissionForVerificationResponse] =
    cisConnector.createSubmissionForVerification(request)

  private def submitVerificationToChris(
    submissionId: Long,
    ua: UserAnswers
  )(implicit request: DataRequest[AnyContent], hc: HeaderCarrier, messages: Messages): Future[ChrisSubmissionResponse] =
    for {
      employerReference <- resolveEmployerReference(request.userId, request.isAgent, request.employerReference)
      chrisRequest      <- chrisVerificationRequestBuilder.build(ua, request.isAgent, employerReference)
      result            <- cisConnector.submitVerificationToChris(submissionId, chrisRequest)
    } yield result

  private def buildCreateSubmissionRequest(
    ua: UserAnswers
  )(implicit messages: Messages): Future[CreateSubmissionForVerificationRequest] =
    CreateSubmissionForVerificationRequestBuilder
      .build(ua)
      .fold(
        error => Future.failed(new RuntimeException(error)),
        request => Future.successful(request)
      )

  def anyUnmatchedResourceRefsStillPresent(
    cisId: String,
    response: GetLastSubmittedVerificationBatchResponse
  )(implicit hc: HeaderCarrier): Future[Boolean] = {

    val unmatchedIds =
      unmatchedSubcontractorIds(response).toSet

    if (unmatchedIds.isEmpty) {
      Future.successful(false)
    } else {
      cisConnector.getSubcontractorList(cisId).map { listResponse =>
        val liveIds =
          listResponse.subcontractors
            .map(_.subcontractorId)
            .toSet

        unmatchedIds.exists(liveIds.contains)
      }
    }
  }

  private def resolveEmployerReference(
    userId: String,
    isAgent: Boolean,
    employerReference: Option[EmployerReference]
  )(implicit hc: HeaderCarrier): Future[EmployerReference] =
    if (isAgent) {
      cisManageService.getAgentClient(userId).flatMap {
        case Some(data) => Future.successful(EmployerReference(data.taxOfficeNumber, data.taxOfficeReference))
        case None       => Future.failed(new RuntimeException("Employer reference missing for agent user"))
      }
    } else {
      employerReference match {
        case Some(er) => Future.successful(er)
        case None     => Future.failed(new RuntimeException("Employer reference missing for non-agent user"))
      }
    }

  private def saveVerificationSubmissionDetailsToSession(
    ua: UserAnswers,
    response: ChrisSubmissionResponse
  ): Future[UserAnswers] = {
    val details = VerificationSubmissionDetailsBuilder.fromSubmissionResponse(
      response,
      LocalDateTime.now(clock.withZone(ukZone))
    )

    ua.set(VerificationSubmissionDetailsPage, details)
      .fold(
        error => Future.failed(error),
        updatedUa => sessionRepository.set(updatedUa).map(_ => updatedUa)
      )
  }

  private def saveVerificationPollDetailsToSession(
    ua: UserAnswers,
    details: VerificationSubmissionDetails
  ): Future[UserAnswers] =
    ua.set(VerificationSubmissionDetailsPage, details)
      .fold(
        error => Future.failed(error),
        updatedUa => sessionRepository.set(updatedUa).map(_ => updatedUa)
      )

  def resetUserAnswers(userAnswers: UserAnswers): Future[Unit] =
    userAnswers.get(CisIdQuery) match {
      case None =>
        logger.warn("CisId not found in session data, skipping UserAnswers reset")
        Future.successful(())

      case Some(cisId) =>
        UserAnswers(userAnswers.id)
          .set(CisIdQuery, cisId)
          .fold(
            _ => {
              logger.error("[VerificationService][resetUserAnswers] - failed to set CisIdQuery on reset userAnswers")
              Future.successful(())
            },
            resetUserAnswers =>
              sessionRepository
                .set(resetUserAnswers)
                .map(_ => ())
                .recover { case ex =>
                  logger.error("[VerificationService][resetUserAnswers] - failed to persist reset userAnswers", ex)
                  ()
                }
          )
    }

  private def required[A](value: Option[A], errorMsg: String): Future[A] =
    value match {
      case Some(v) => Future.successful(v)
      case None    => Future.failed(new RuntimeException(errorMsg))
    }

  def recreateCurrentBatchFromUnmatchedVerifications(
    cisId: String,
    userAnswers: UserAnswers
  )(implicit hc: HeaderCarrier): Future[UserAnswers] =
    userAnswers.get(CurrentVerificationBatchResponsePage) match {
      case Some(_) =>
        Future.successful(userAnswers)
      case None    =>
        initialiseCurrentBatchFromUnmatchedVerifications(cisId, userAnswers)
    }

  private def initialiseCurrentBatchFromUnmatchedVerifications(
    cisId: String,
    userAnswers: UserAnswers
  )(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      lastSubmitted <- userAnswers
                         .get(LastSubmittedVerificationBatchResponsePage)
                         .map(Future.successful)
                         .getOrElse(
                           Future.failed(
                             new RuntimeException(
                               "LastSubmittedVerificationBatchResponsePage not found in session data"
                             )
                           )
                         )

      submittedSubbieRefs <- {
        val refs = unmatchedSubcontractorRefs(lastSubmitted)

        if (refs.nonEmpty) Future.successful(refs)
        else
          Future.failed(
            new RuntimeException(
              "No unmatched subcontractor references found in LastSubmittedVerificationBatchResponsePage"
            )
          )
      }

      latestUa <- refreshNewestVerificationBatch(userAnswers)

      _ <-
        if (latestBatchCanBeModified(latestUa)) {
          for {
            refreshedUa <- getCurrentVerificationBatch(latestUa)

            current <- refreshedUa
                         .get(CurrentVerificationBatchResponsePage)
                         .map(Future.successful)
                         .getOrElse(
                           Future.failed(
                             new RuntimeException(
                               "CurrentVerificationBatchResponsePage not found in session data"
                             )
                           )
                         )

            verificationBatchRef <-
              current.verificationBatch
                .flatMap(_.verifBatchResourceRef)
                .map(Future.successful)
                .getOrElse(
                  Future.failed(
                    new RuntimeException("Missing verifBatchResourceRef in current verification batch")
                  )
                )

            existingVerificationRefs =
              current.verifications
                .flatMap(_.verificationResourceRef)
                .distinct

            modifyRequest =
              ModifyVerificationsRequest(
                instanceId = cisId,
                deleteVerifications =
                  if (existingVerificationRefs.nonEmpty)
                    Some(DeleteVerifications(existingVerificationRefs))
                  else None,
                createVerifications = Some(
                  CreateVerifications(
                    verificationBatchRef,
                    submittedSubbieRefs
                  )
                )
              )

            _ <- cisConnector.modifyVerificationBatch(modifyRequest)
          } yield ()
        } else if (latestBatchCanBeRecreated(latestUa)) {
          cisConnector
            .createVerificationBatchAndVerifications(
              CreateVerificationBatchAndVerificationsRequest(
                instanceId = cisId,
                verificationResourceReferences = submittedSubbieRefs,
                actionIndicator = None
              )
            )
            .map(_ => ())
        } else {
          Future.failed(
            new RuntimeException("Latest verification batch status does not allow unmatched batch recreation")
          )
        }

      afterCurrent <- getCurrentVerificationBatch(latestUa)
      afterNewest  <- refreshNewestVerificationBatch(afterCurrent)
      _            <- sessionRepository.set(afterNewest)

    } yield afterNewest

  private def unmatchedSubcontractorRefs(
    response: GetLastSubmittedVerificationBatchResponse
  ): Seq[Long] = {

    val ids = unmatchedSubcontractorIds(response)

    response.subcontractors
      .filter(sub => ids.contains(sub.subcontractorId))
      .flatMap(_.subbieResourceRef)
      .distinct
  }

  private def unmatchedSubcontractorIds(
    response: GetLastSubmittedVerificationBatchResponse
  ): Seq[Long] =
    response.verifications.collect {
      case verification
          if CheckUnmatchedSubcontractorsService.isUnmatched(verification) &&
            verification.subcontractorId.isDefined =>
        verification.subcontractorId.get
    }.distinct

  def proceedInsufficientVerification(cisId: String, subcontractorId: Long, batch: GetCurrentVerificationBatchResponse)(
    implicit hc: HeaderCarrier
  ): Future[Unit] =
    proceedVerification(cisId, subcontractorId, batch, cisConnector.proceedInsufficientVerification)

  def proceedUnmatchedVerification(cisId: String, subcontractorId: Long, batch: GetCurrentVerificationBatchResponse)(
    implicit hc: HeaderCarrier
  ): Future[Unit] =
    proceedVerification(cisId, subcontractorId, batch, cisConnector.proceedUnmatchedVerification)

  private def proceedVerification(
    cisId: String,
    subcontractorId: Long,
    batch: GetCurrentVerificationBatchResponse,
    proceed: ProceedVerificationRequest => Future[Unit]
  ): Future[Unit] =
    (
      for {
        verificationBatchResourceRef <- batch.verificationBatch.flatMap(_.verifBatchResourceRef)
        verificationResourceRef      <- batch.verifications
                                          .find(_.subcontractorId.contains(subcontractorId))
                                          .flatMap(_.verificationResourceRef)
      } yield ProceedVerificationRequest(
        instanceId = cisId,
        verificationBatchResourceRef = verificationBatchResourceRef,
        verificationResourceRef = verificationResourceRef
      )
    ) match {
      case Some(request) =>
        proceed(request)

      case None =>
        Future.failed(
          new RuntimeException(
            s"Unable to proceed verification. Missing resource refs for subcontractorId=$subcontractorId"
          )
        )
    }

  def refreshVerificationBatches(
    userAnswers: UserAnswers
  )(implicit hc: HeaderCarrier): Future[UserAnswers] =
    for {
      afterCurrent <- getCurrentVerificationBatch(userAnswers)
      afterNewest  <- refreshNewestVerificationBatch(afterCurrent)
      _            <- sessionRepository.set(afterNewest)
    } yield afterNewest
}
