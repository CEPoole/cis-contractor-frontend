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

package views.verify

import base.SpecBase
import config.FrontendAppConfig
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import play.api.Application
import play.api.i18n.{Lang, Messages, MessagesApi, MessagesImpl}
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import viewmodels.verify.VerificationRequestSubmittedViewModel
import views.html.verify.VerificationRequestSubmittedView

import java.time.LocalDateTime
import utils.DateTimeFormats
import java.time.format.DateTimeFormatter

class VerificationRequestSubmittedViewSpec extends SpecBase with GuiceOneAppPerSuite {

  "VerificationRequestSubmittedView" - {

    "render the page correctly when reverify list and email are present" in new Setup {

      val doc: Document = Jsoup.parse(html.toString())

      doc.title must include(
        messages("verify.verificationRequestSubmitted.title")
      )

      doc.select(".govuk-panel__title").text mustBe
        messages("verify.verificationRequestSubmitted.heading")

      val recruitmentBanner = doc.select(".hmrc-user-research-banner")

      recruitmentBanner.isEmpty mustBe false
      recruitmentBanner.select("a").attr("href") must include(appConfig.userResearchUrl)

      doc.select(".govuk-panel__body").text must include(referenceNumber)

      doc.text must include(
        messages(
          "verify.verificationRequestSubmitted.submittedAt",
          submittedAt.format(DateTimeFormatter.ofPattern("HH:mm")),
          submittedAt.format(DateTimeFormats.dateTimeFormat()(messages.lang))
        )
      )

      doc.select("h2").text must include(
        messages("verify.verificationRequestSubmitted.details.subHeading")
      )

      subcontractorsToVerify.foreach { name =>
        doc.select("ul.govuk-list--bullet").text must include(name)
      }

      subcontractorsToReverify.foreach { name =>
        doc.select("ul.govuk-list--bullet").text must include(name)
      }

      doc.select("p.govuk-body").text must include(email)

      val emailVerificationLink: Elements =
        doc.select(s"a[href='${appConfig.verificationHistoryUrl}']")

      emailVerificationLink.text must include(
        messages("verify.verificationRequestSubmitted.email.verification.link")
      )

      doc.select(".govuk-inset-text").text must include(
        messages("verify.verificationRequestSubmitted.print.text")
      )

      doc.select(".govuk-inset-text").text must include(
        messages("verify.verificationRequestSubmitted.print.link")
      )

      doc.select("h2").text must include(
        messages("verify.verificationRequestSubmitted.needHelp.subHeading")
      )

      val manageLink: Elements =
        doc.select(s"a[href='${appConfig.manageSubcontractorsUrl}/$cisId']")

      manageLink.size() mustBe 1
      manageLink.text() mustBe
        messages("verify.verificationRequestSubmitted.needHelp.manageSubcontractors.link")

      doc.select("h2").text must include(
        messages("verify.verificationRequestSubmitted.feedback.subHeading")
      )

      val surveyLink: Elements =
        doc.select(s"a[href='${appConfig.cisFeedbackSurveyUrl}']")

      surveyLink.text() mustBe messages("verify.verificationRequestSubmitted.feedback.survey.link")
      surveyLink.attr("href") mustBe appConfig.cisFeedbackSurveyUrl
    }

    "must not render the recruitment banner when user research banner flag is disabled" in new SetupWithBannerDisabled {

      val doc: Document = Jsoup.parse(html.toString())

      doc.select(".hmrc-user-research-banner").isEmpty mustBe true
    }

    "not render reverify section or email paragraph when both are absent" in new Setup {

      override val viewModel: VerificationRequestSubmittedViewModel =
        viewModel.copy(
          subcontractorsToReverify = Seq.empty,
          confirmationEmail = None
        )

      val doc: Document = Jsoup.parse(html.toString())

      doc.text must not include
        messages("verify.verificationRequestSubmitted.subcontractorsToReverify.label")

      doc.text must not include email
    }
  }

  trait Setup {

    implicit val request: Request[_] =
      FakeRequest()

    implicit val messages: Messages =
      MessagesImpl(
        Lang.defaultLang,
        app.injector.instanceOf[MessagesApi]
      )

    val appConfig: FrontendAppConfig =
      app.injector.instanceOf[FrontendAppConfig]

    val view: VerificationRequestSubmittedView =
      app.injector.instanceOf[VerificationRequestSubmittedView]

    val referenceNumber            = "Reference number 12345"
    val submittedAt: LocalDateTime = LocalDateTime.of(2026, 4, 27, 10, 30)

    val subcontractorsToVerify: Seq[String] =
      Seq(
        "Brody, Martin",
        "Hooper And Associates",
        "Quint Transportation",
        "The Kintner Group"
      )

    val subcontractorsToReverify: Seq[String] =
      Seq(
        "Grant, Alan",
        "InGen Research"
      )

    val email = "test@testmail.com"

    val cisId = "1"

    val viewModel: VerificationRequestSubmittedViewModel =
      VerificationRequestSubmittedViewModel(
        manageSubcontractorsUrl = s"${applicationConfig.manageSubcontractorsUrl}/$cisId",
        verificationHistoryUrl = applicationConfig.verificationHistoryUrl,
        referenceNumber = Some(referenceNumber),
        submittedAt = submittedAt,
        subcontractorsToVerify = subcontractorsToVerify,
        subcontractorsToReverify = subcontractorsToReverify,
        confirmationEmail = Some(email)
      )

    lazy val html: HtmlFormat.Appendable =
      view(viewModel)
  }

  trait SetupWithBannerDisabled {

    val app: Application =
      applicationBuilder()
        .configure("features.user-research-banner-enabled" -> false)
        .build()

    val view: VerificationRequestSubmittedView = app.injector.instanceOf[VerificationRequestSubmittedView]

    implicit val request: Request[_] = FakeRequest()

    implicit val messages: Messages =
      MessagesImpl(
        Lang.defaultLang,
        app.injector.instanceOf[MessagesApi]
      )

    val appConfig: FrontendAppConfig = app.injector.instanceOf[FrontendAppConfig]

    val referenceNumber            = "Reference number 12345"
    val submittedAt: LocalDateTime = LocalDateTime.of(2026, 4, 27, 10, 30)

    val subcontractorsToVerify: Seq[String] =
      Seq(
        "Brody, Martin",
        "Hooper And Associates",
        "Quint Transportation",
        "The Kintner Group"
      )

    val subcontractorsToReverify: Seq[String] =
      Seq(
        "Grant, Alan",
        "InGen Research"
      )

    val email = "test@testmail.com"
    val cisId = "1"

    val viewModel: VerificationRequestSubmittedViewModel =
      VerificationRequestSubmittedViewModel(
        manageSubcontractorsUrl = s"${appConfig.manageSubcontractorsUrl}/$cisId",
        verificationHistoryUrl = appConfig.verificationHistoryUrl,
        referenceNumber = Some(referenceNumber),
        submittedAt = submittedAt,
        subcontractorsToVerify = subcontractorsToVerify,
        subcontractorsToReverify = subcontractorsToReverify,
        confirmationEmail = Some(email)
      )

    lazy val html: HtmlFormat.Appendable = view(viewModel)
  }
}
