package http

import akka.http.scaladsl.marshalling.ToResponseMarshallable.apply
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directive.addByNameNullaryApply
import akka.http.scaladsl.server.{Directive0, Directives, Route}

trait CorsSupport extends Directives {

  private val corsResponseHeaders: List[ModeledHeader] = List[ModeledHeader](
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Credentials`(true),
    `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With", "x-api-key")
  )

  def corsHandler(r: Route): Route = addAccessControlHeaders {
    preflightRequestHandler ~ r
  }

  private def addAccessControlHeaders: Directive0 =
    respondWithHeaders(corsResponseHeaders)

  //this handles preflight OPTIONS requests.
  private def preflightRequestHandler: Route = options {
    complete {
      HttpResponse(StatusCodes.OK)
        .withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE))
    }
  }

}
