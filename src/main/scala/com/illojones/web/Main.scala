package com.illojones.web

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.illojones.web.plackey.{PlackeyMessages, Plactor}
import com.illojones.web.slack.SlackMessages
import com.illojones.web.slack.casbot.Casbot
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._


object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem("system")
  implicit val actorMaterializer: ActorMaterializer = ActorMaterializer()

  val cf = ConfigFactory.load()
  val casbotToken = cf.getString("casbot.token")

  val casbot = actorSystem.actorOf(Props[Casbot], "casbot")

  final val ErrorResponse = "¯\\_(ツ)_/¯"

  def slackResponse(text: String) = {
    val msg = s"""
       |{
       |  "text": "$text"
       |}
     """.stripMargin
    complete(msg)
  }

  val route =
    path(cf.getString("casbot.path")) {
      post {
        formFields('token, 'team_id, 'team_domain, 'channel_id, 'channel_name, 'timestamp, 'user_id,
          'user_name, 'text).as(SlackMessages.IncomingMessage) { msg ⇒
          implicit val askTimeout: Timeout = 5.seconds
          msg.token match {
            case `casbotToken` ⇒
              onSuccess((casbot ? msg).mapTo[SlackMessages.Response]) { resp ⇒ complete(resp.text) }
            case _ ⇒ slackResponse(ErrorResponse)
          }
        } ~ {
          slackResponse(ErrorResponse)
        }
      }
    } ~ path(cf.getString("plackey.path")) {
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
