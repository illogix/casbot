package com.illojones.web.slack

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.illojones.web.slack.SlackMessages._
import spray.json.DefaultJsonProtocol

object SlackMessages {
  case class CassieMessage(user: String, channel: String, text: String)

  case class Team(id: String, name: String, domain: String)
  case class Self(id: String, name: String)
  case class ConnectResponse(ok: Boolean, url: String, team: Team, self: Self)

  case class OutgoingMessage(id: String, channel: String, text: String, `type`: String = "message")
}

trait SlackMessageSerialization extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val OutgoingMessageFormat = jsonFormat4(OutgoingMessage)
  implicit val TeamFormat = jsonFormat3(Team)
  implicit val SelfFormat = jsonFormat2(Self)
  implicit val ConnectResponseFormat = jsonFormat4(ConnectResponse)
}

object SlackEvents {
  sealed trait Event

  sealed trait MessageEvent extends Event

  case class RegularMessage(channel: String, user: String, text: String, ts: Double, sourceTeam: String, team: String)
    extends MessageEvent

  case class SlackbotResponseMessage(channel: String, user: String, text: String, eventTs: Double, ts: Double)
    extends MessageEvent

  case object UnhandledEvent extends Event

}

