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

package views.contractordetails

import base.SpecBase
import config.FrontendAppConfig
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalatest.matchers.must.Matchers
import play.api.Application
import play.api.i18n.Messages
import play.api.test.FakeRequest
import play.twirl.api.HtmlFormat
import views.html.contractordetails.ContractorDetailsUpdatedView

class ContractorDetailsUpdatedViewSpec extends SpecBase with Matchers {

  "ContractorDetailsUpdatedView" - {

    Seq(
      ("AGENT", applicationConfig.constructionIndustryAgentAccountUrl + "1"),
      ("ORGANISATION", applicationConfig.constructionIndustryOrgAccountUrl)
    ).foreach { case (accountTypeSTR, cisAccountUrl) =>
      s"when accountType is '$accountTypeSTR'" - {

        "must render the page with correct heading, paragraphs, and other contents" in new Setup {
          val html: HtmlFormat.Appendable = view(cisAccountUrl)
          val doc: Document               = Jsoup.parse(html.body)

          doc.title must include(messages("contractordetails.contractorDetailsUpdated.title"))

          doc.select("h1").text must include(messages("contractordetails.contractorDetailsUpdated.heading"))

          doc.select("p").text must include(
            messages("contractordetails.contractorDetailsUpdated.p1.details.prefix")
          )

          doc
            .select(
              s"a[href=${controllers.contractordetails.routes.ContractorDetailsCheckAnswersController.onPageLoad().url}]"
            )
            .size mustBe 1

          doc.select("a").text must include(messages("contractordetails.contractorDetailsUpdated.p1.details.link"))

          doc.select("a").text must include(
            messages("contractordetails.contractorDetailsUpdated.returnToDashboard.link")
          )

          doc
            .select(s"a[href='${appConfig.cisFeedbackSurveyUrl}']")
            .text must include(messages("contractordetails.contractorDetailsUpdated.p2.whatDidYouThink.link"))

          doc.select("a").text must include(
            messages("contractordetails.contractorDetailsUpdated.p2.whatDidYouThink.link")
          )

          doc.select("p").text must include(
            messages("contractordetails.contractorDetailsUpdated.p2.whatDidYouThink.suffix")
          )
        }

        "must not show back link" in new Setup {
          val html: HtmlFormat.Appendable = view(cisAccountUrl)
          val doc: Document               = Jsoup.parse(html.body)

          doc.getElementsByClass("govuk-back-link").size mustBe 0
        }
      }
    }

    trait Setup {
      val app: Application                          = applicationBuilder().build()
      val view: ContractorDetailsUpdatedView        = app.injector.instanceOf[ContractorDetailsUpdatedView]
      val appConfig: FrontendAppConfig              = app.injector.instanceOf[FrontendAppConfig]
      implicit val request: play.api.mvc.Request[_] = FakeRequest()
      implicit val messages: Messages               = play.api.i18n.MessagesImpl(
        play.api.i18n.Lang.defaultLang,
        app.injector.instanceOf[play.api.i18n.MessagesApi]
      )
    }
  }
}
