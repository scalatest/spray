/*
 * Copyright (C) 2011 Mathias Doenitz
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

package cc.spray
package connectors

import org.eclipse.jetty.continuation._
import collection.JavaConversions._
import collection.mutable.HashMap
import utils.ActorHelpers._
import akka.util.Logging
import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}
import utils.CantWriteResponseBodyException

class Jetty7Connector extends HttpServlet with Logging {
  
  val rootService = actor("spray-root-service")
  
  override def init() {
    log.info("Initializing Jetty 7 <=> Spray Connector")
  }

  override def service(req: HttpServletRequest, resp: HttpServletResponse) {
    log.slf4j.debug("Processing HttpServletRequest {}", req)
    rootService ! RawRequestContext(createRawRequest(req), suspend(req, resp)) 
  }
  
  def createRawRequest(req: HttpServletRequest) = new RawRequest {
    def method = req.getMethod
    lazy val uri = {
      val buffer = req.getRequestURL
      val queryString = req.getQueryString
      if (queryString != null && queryString.length > 1) buffer.append('?').append(queryString)
      buffer.toString
    }
    lazy val headers = {
      val map = HashMap.empty[String, String]
      for (name <- req.getHeaderNames.toList; value <- req.getHeaders(name).toList) {
        map.update(name, value)
      }
      map
    }
    def inputStream = req.getInputStream
    def remoteIP = req.getRemoteAddr
    def protocol = req.getProtocol
  }

  def createRawResponse(resp: HttpServletResponse) = new RawResponse {
    def setStatus(code: Int) { resp.setStatus(code) }
    def addHeader(name: String, value: String) { resp.addHeader(name, value) }
    def outputStream = resp.getOutputStream
  }
  
  def suspend(req: HttpServletRequest, resp: HttpServletResponse): (RawResponse => Unit) => Unit = {
    val continuation = ContinuationSupport.getContinuation(req)
    continuation.addContinuationListener(new ContinuationListener {
      def onTimeout(continuation: Continuation) {
        log.slf4j.warn("Time out of request: {}", req)
        TimeOutHandler.get.apply(
          createRawRequest(req),
          createRawResponse(resp)
        )
        continuation.complete()
      }
      def onComplete(continuation: Continuation) {}
    })
    continuation.setTimeout(Settings.AsyncTimeout)
    continuation.suspend(resp)    
    
    { completer =>
      completer(createRawResponse(resp))
      try {
        continuation.complete()
      } catch {
        case e: CantWriteResponseBodyException => {
          log.slf4j.error("Could not write response body, " +
                  "probably the request has either timed out or the client has disconnected")
        }
        case e: Exception => log.slf4j.error("Could not complete request", e)
      }
    }
  }
  
}