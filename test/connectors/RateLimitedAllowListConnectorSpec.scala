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

package connectors

import models.AllowListCheckResponse
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{verify, when}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.client.{HttpClientV2, RequestBuilder}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.net.URL
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RateLimitedAllowListConnectorSpec extends AnyWordSpec with Matchers with MockitoSugar with ScalaFutures {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "RateLimitedAllowListConnector.checkAllowList" should {

    "return true when the service responds with included: true" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("rate-limited-allow-list")).thenReturn("http://allow-list-host")
      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[AllowListCheckResponse](any(), any()))
        .thenReturn(Future.successful(AllowListCheckResponse(included = true)))

      val connector = new RateLimitedAllowListConnector(config, http)

      connector.checkAllowList("my-service", "my-feature", "754/EZ00047").futureValue mustBe true
    }

    "return false when the service responds with included: false" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("rate-limited-allow-list")).thenReturn("http://allow-list-host")
      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[AllowListCheckResponse](any(), any()))
        .thenReturn(Future.successful(AllowListCheckResponse(included = false)))

      val connector = new RateLimitedAllowListConnector(config, http)

      connector.checkAllowList("my-service", "my-feature", "754/EZ00047").futureValue mustBe false
    }

    "POST to the correct URL with the identifier in the request body" in {
      val config = mock[ServicesConfig]
      val http   = mock[HttpClientV2]
      val rb     = mock[RequestBuilder]

      when(config.baseUrl("rate-limited-allow-list")).thenReturn("http://allow-list-host")
      when(http.post(any())(any())).thenReturn(rb)
      when(rb.withBody(any[JsValue]())(any(), any(), any())).thenReturn(rb)
      when(rb.execute[AllowListCheckResponse](any(), any()))
        .thenReturn(Future.successful(AllowListCheckResponse(included = true)))

      val connector = new RateLimitedAllowListConnector(config, http)

      connector.checkAllowList("cis-management-frontend", "cis-private-beta-2026", "754/EZ00047").futureValue

      val urlCaptor: ArgumentCaptor[URL] = ArgumentCaptor.forClass(classOf[URL])
      verify(http).post(urlCaptor.capture())(any[HeaderCarrier])
      urlCaptor.getValue.toString mustBe
        "http://allow-list-host/rate-limited-allow-list/services/cis-management-frontend/features/cis-private-beta-2026"

      val bodyCaptor: ArgumentCaptor[JsValue] = ArgumentCaptor.forClass(classOf[JsValue])
      verify(rb).withBody(bodyCaptor.capture())(any(), any(), any())
      bodyCaptor.getValue mustBe Json.obj("identifier" -> "754/EZ00047")
    }
  }
}
