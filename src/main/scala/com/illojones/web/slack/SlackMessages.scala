package com.illojones.web.slack

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.illojones.web.slack.SlackMessages._
import spray.json.DefaultJsonProtocol

object SlackMessages {
  case class IncomingMessage(token: String, team_id: String, team_domain: String, channel_id: String,
                             channel_name: String, timestamp: String, user_id: String,
                             user_name: String, text: String)
  case class Response(text: String)

  case class CassieMessage(user: String, channel: String, text: String)

  case class Team(id: String, name: String, domain: String)
  case class Self(id: String, name: String)
  case class ConnectResponse(ok: Boolean, url: String, team: Team, self: Self)

  case class RtmMessage(`type`: Option[String], presence: Option[String], user: Option[String], channel: Option[String],
                        text: Option[String], ts: Option[String], source_team: Option[String], team: Option[String],
                        url: Option[String], title: Option[String], subtitle: Option[String], msg: Option[String],
                        content: Option[String], launchUri: Option[String], avatarImage: Option[String],
                        ssbFilename: Option[String], imageUri: Option[String], is_shared: Option[Boolean],
                        event_ts: Option[String], ok: Option[Boolean], reply_to: Option[String])

  case class OutgoingMessage(id: String, channel: String, text: String, `type`: String = "message")
}

trait SlackMessageSerialization extends SprayJsonSupport with DefaultJsonProtocol {
  // One of the built-in spray-json auto-formatters
  implicit val RtmMessageFormat = jsonFormat21(RtmMessage)
  implicit val OutgoingMessageFormat = jsonFormat4(OutgoingMessage)
  implicit val TeamFormat = jsonFormat3(Team)
  implicit val SelfFormat = jsonFormat2(Self)
  implicit val ConnectResponseFormat = jsonFormat4(ConnectResponse)
}

