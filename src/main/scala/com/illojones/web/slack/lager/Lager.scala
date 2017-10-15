package com.illojones.web.slack.lager

import java.sql.Timestamp

import akka.actor.{Actor, ActorLogging, Props}
import com.illojones.web.plackey.database.DatabaseUtil
import com.illojones.web.plackey.database.DatabaseUtil.Log
import com.illojones.web.slack.SlackEvents.{RegularMessage, SlackbotResponseMessage}
import com.typesafe.config.Config

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure

object Lager {
  def props(config: Config) = Props(new Lager(config))
}

class Lager(config: Config) extends Actor with ActorLogging {
  val db = DatabaseUtil.getDatabase(DatabaseUtil.dbConnectionUrl(config.getString("plackey.dbHost"),
    config.getString("plackey.dbDb"), config.getString("plackey.dbUser"), config.getString("plackey.dbPassword")))

  override def preStart(): Unit = {
    super.preStart()

    DatabaseUtil.createTableIfNeeded(db)
  }

  private def d2ts(d: Double): Timestamp = new Timestamp((d*1000).toLong)

  private def addlog(msg: Log) = {
    DatabaseUtil.addLog(db, msg) onComplete {
      case Failure(e) ⇒ log.error(s"Failed to add log: ${e.getMessage}")
      case _ ⇒
    }
  }

  override def receive = {
    case RegularMessage(channel, user, text, ts, sourceTeam, team) ⇒
      log.debug(s"RegularMessage($channel, $user, $text, $ts, $sourceTeam, $team)")
      val logMsg = Log(None, channel, user, d2ts(ts), text)

      addlog(logMsg)

    case SlackbotResponseMessage(channel, user, text, eventTs, ts) ⇒
      log.debug(s"SlackbotResponseMessage($channel, $user, $text, $eventTs, $ts)")
      val logMsg = Log(None, channel, user, d2ts(ts), text)

      addlog(logMsg)

    case _ ⇒
  }
}
