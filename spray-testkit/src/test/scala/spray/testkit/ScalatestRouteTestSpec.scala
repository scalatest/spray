/*
 * Copyright © 2011-2013 the spray project <http://spray.io>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.testkit

import org.scalatest._
import exceptions.TestFailedException
import akka.testkit.TestProbe
import spray.routing.{ MethodRejection, RequestContext, Directives }
import spray.http._
import HttpMethods._
import MediaTypes._
import HttpCharsets._
import StatusCodes._
import HttpHeaders._

class ScalatestRouteTestSpec extends FreeSpec with MustMatchers with Directives with ScalatestRouteTest {

  "The ScalatestRouteTest must" - {
    "support" - {

      "the most simple and direct route test" in {
        Get() ~> {
          (_: RequestContext).complete(HttpResponse())
        } ~> (_.response) mustEqual HttpResponse()
      }

      "a test using a directive and some checks" in {
        val pinkHeader = RawHeader("Fancy", "pink")
        Get() ~> addHeader(pinkHeader) ~> {
          respondWithHeader(pinkHeader) { complete("abc") }
        } ~> check {
          status mustEqual OK
          body mustEqual HttpEntity(ContentType(`text/plain`, `UTF-8`), "abc")
          header("Fancy") mustEqual Some(pinkHeader)
        }
      }

      "proper rejection collection" in {
        Post("/abc", "content") ~> {
          (get | put) {
            complete("naah")
          }
        } ~> check {
          rejections mustEqual List(MethodRejection(GET), MethodRejection(PUT))
        }
      }

      "separate running route from checking" in {
        val pinkHeader = RawHeader("Fancy", "pink")

        case class HandleRequest(ctx: RequestContext)
        val service = TestProbe()
        val handler = TestProbe()

        val result =
          Get() ~> addHeader(pinkHeader) ~> {
            respondWithHeader(pinkHeader) { ctx ⇒ service.send(handler.ref, HandleRequest(ctx)) }
          } ~> runRoute

        val ctx = handler.expectMsgType[HandleRequest].ctx
        ctx.complete("abc")

        check {
          status mustEqual OK
          body mustEqual HttpEntity(ContentType(`text/plain`, `UTF-8`), "abc")
          header("Fancy") mustEqual Some(pinkHeader)
        }(result)
      }
    }

    "produce TestFailedExceptions with the proper stackdepth" - {

      def thisLineNumber = {
        val st = Thread.currentThread.getStackTrace

        if (!st(2).getMethodName.contains("thisLineNumber"))
          st(2).getLineNumber
        else
          st(3).getLineNumber
      }

      val testRoute =
        path("testing") {
          get {
            respondWithMediaType(`text/html`) {
              complete {
                <html>
                  <body>
                    <h1>Testing 1, 2, 3</h1>
                  </body>
                </html>
              }
            }
          }
        }

      "when an assertion fails" in {
        the[TestFailedException] thrownBy {
          Get("/testing") ~> testRoute ~> check {
            responseAs[String] must include("2, 3, 4")
          }
        } must have('failedCodeLineNumber(Some(thisLineNumber - 2)))
      }

      "when a request is not handled" in {
        the[TestFailedException] thrownBy {
          Get("/testing123") ~> testRoute ~> check {
            responseAs[String] must include("2, 3, 4")
          }
        } must have('failedCodeLineNumber(Some(thisLineNumber - 2)))
      }

      "when a request that was expected to be rejected was not rejected" in {
        the[TestFailedException] thrownBy {
          Get("/testing") ~> testRoute ~> check {
            // The call to "rejections" should blow up because GET /testing is a good request
            rejections must have size 1
          }
        } must have('failedCodeLineNumber(Some(thisLineNumber - 2)))
      }

      "when a request was rejected with a list of somethings" in {
        the[TestFailedException] thrownBy {
          Post("/testing") ~> testRoute ~> check {
            // The call to "responseAs" should blow up because POST /testing is a bad request (wrong method, only GET supported)
            responseAs[String] must include("1, 2, 3")
          }
        } must have('failedCodeLineNumber(Some(thisLineNumber - 2)))
      }

      "when a route is completed/rejected more than once" is pending // saveResult method

      "when a request times out" is pending // failNotCompletedNotRejected method
    }
  }
}

