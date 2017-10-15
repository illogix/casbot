package com.illojones.web.slack

import akka.actor.{Actor, ActorLogging, Props}
import akka.http.scaladsl.model.ws.TextMessage
import com.illojones.web.slack.SlackEvents.{RegularMessage, SlackbotResponseMessage}
import com.illojones.web.slack.cassie.Cassie
import com.illojones.web.slack.MessageHandler._
import com.illojones.web.slack.lager.Lager
import com.typesafe.config.Config
import spray.json.{JsValue, JsonParser, ParserInput}

import scala.util.Try

object MessageHandler {
  def props(config: Config) = Props(new MessageHandler(config))

  private def stripQuotes(jsVal: JsValue) = {
    val s = jsVal.toString()
    if (s.startsWith("\"") && s.endsWith("\"") && s.length > 1) s.substring(1, s.length - 1) else s
  }

  implicit class IntWithTimes(fields: Map[String, JsValue]) {
    def getString(key: String): Option[String] = fields.get(key).map(stripQuotes)

    def getDouble(key: String): Option[Double] = getString(key).flatMap(l ⇒ Try(l.toDouble).toOption)
  }
}

class MessageHandler(config: Config) extends Actor with ActorLogging {

  private val cassie = context.actorOf(Cassie.props(config), "cassie")

  private val lager = context.actorOf(Lager.props(config), "lager")

  override def receive = {

    case m: TextMessage.Strict ⇒
      val jsFields = JsonParser(ParserInput(m.text)).asJsObject.fields

      jsFields.getString("type") match {
        case Some("message") if jsFields.get("subtype").isEmpty ⇒
          val rm = for {
            channel ← jsFields.getString("channel")
            user ← jsFields.getString("user")
            text ← jsFields.getString("text")
            ts ← jsFields.getDouble("ts")
            sourceTeam ← jsFields.getString("source_team")
            team ← jsFields.getString("team")
          } yield {
            RegularMessage(channel, user, text, ts, sourceTeam, team)
          }

          rm foreach { msg ⇒
            cassie ! msg
            lager ! msg
          }

        case Some("message") if jsFields.getString("subtype").contains("slackbot_response") ⇒
          val msg = for {
            channel ← jsFields.getString("channel")
            user ← jsFields.getString("user")
            text ← jsFields.getString("text")
            eventTs ← jsFields.getDouble("event_ts")
            ts ← jsFields.getDouble("ts")
          } yield {
            SlackbotResponseMessage(channel, user, text, eventTs, ts)
          }

          msg foreach { m ⇒
            lager ! m
          }

        case _ ⇒ // Unhandled

      }

    case _ ⇒ log.debug(s"Unhandled message")
  }
}
