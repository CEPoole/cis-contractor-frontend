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

import models.{Subcontractor, UserAnswers}
import models.verify.VerificationBatchStatus
import models.verify.VerificationBatchStatus.*
import pages.verify.NewestVerificationBatchResponsePage

import javax.inject.{Inject, Singleton}

@Singleton
class VerificationPreSelectionService @Inject() () {

  def preSelectedSubcontractorIds(
    displayedSubcontractors: Seq[Subcontractor],
    userAnswers: UserAnswers
  ): Set[String] =
    if (hasOpenCurrentVerificationBatch(userAnswers)) {
      preSelectedFromNewestBatch(displayedSubcontractors, userAnswers)
    } else {
      displayedSubcontractors
        .take(100)
        .map(_.subcontractorId.toString)
        .toSet
    }

  private def hasOpenCurrentVerificationBatch(userAnswers: UserAnswers): Boolean =
    userAnswers
      .get(NewestVerificationBatchResponsePage)
      .flatMap(_.verificationBatch)
      .flatMap(_.status)
      .flatMap(VerificationBatchStatus.from)
      .exists {
        case Started | Validated => true
        case _                   => false
      }

  private def preSelectedFromNewestBatch(
    displayedSubcontractors: Seq[Subcontractor],
    userAnswers: UserAnswers
  ): Set[String] = {
    val verificationResourceRefs =
      userAnswers
        .get(NewestVerificationBatchResponsePage)
        .map(_.verifications.flatMap(_.verificationResourceRef).toSet)
        .getOrElse(Set.empty[Long])

    displayedSubcontractors
      .filter(_.subbieResourceRef.exists(verificationResourceRefs.contains))
      .map(_.subcontractorId.toString)
      .toSet
  }
}
