package com.illojones.web

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.Timeout
import com.illojones.web.plackey.{PlackeyMessages, Plactor}
import com.illojones.web.slack.Bot
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._


object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem("system")
  final implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))

  val cf = ConfigFactory.load()
  val bot = actorSystem.actorOf(Bot.props(cf), "bot")

  final val ErrorResponse = "¯\\_(ツ)_/¯"

  val route =
    path(cf.getString("plackey.path")) {
      getFromResource("plackey/search.html") ~
        post {
          formFields('name.?, 'after.?, 'before.?, 'channel.?, 'text.?, 'regex.?, 'limit.?, 'context.?)
            .as(PlackeyMessages.Query) { q ⇒
              implicit val askTimeout: Timeout = 30.seconds
              val actor = actorSystem.actorOf(Props[Plactor])
              onSuccess((actor ? q).mapTo[PlackeyMessages.Response]) { resp =>
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, resp.text))
              }
            } ~ {
            complete(ErrorResponse)
          }
        }
    }

  complete(ErrorResponse)

  Http().bindAndHandle(route, "localhost", cf.getInt("slack.port"))

}
