package com.illojones.web.slack.lager

import java.sql.Timestamp

import akka.actor.{Actor, ActorLogging, PoisonPill, Props, Stash}
import com.illojones.web.plackey.database.DatabaseUtil
import com.illojones.web.plackey.database.DatabaseUtil.{Channel, Log, User}
import com.illojones.web.slack.Bot
import com.illojones.web.slack.MessageHandler.RichJsFields
import com.illojones.web.slack.SlackEvents.{RegularMessage, SlackbotResponseMessage}
import com.illojones.web.slack.lager.Lager._
import com.typesafe.config.Config
import spray.json.{JsArray, JsObject}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Lager {
  def props(config: Config) = Props(new Lager(config))

  case object Ready
}

class Lager(config: Config) extends Actor with ActorLogging with Stash {
  val db = DatabaseUtil.getDatabase(DatabaseUtil.dbConnectionUrl(config.getString("plackey.dbHost"),
    config.getString("plackey.dbDb"), config.getString("plackey.dbUser"), config.getString("plackey.dbPassword")))

  private val usersUrl = config.getString("slack.usersUrl")
  private val channelsUrl = config.getString("slack.channelsUrl")

  override def preStart(): Unit = {
    super.preStart()

    DatabaseUtil.createTablesIfNeeded(db).onComplete {
      case Success(_) ⇒
        context.parent ! Bot.ApiRequest(usersUrl)
        context.parent ! Bot.ApiRequest(channelsUrl)

        self ! Ready
      case Failure(_) ⇒
        log.error("Failed to create table!")
        self ! PoisonPill
    }
  }

  private def d2ts(d: Double): Timestamp = new Timestamp((d*1000).toLong)

  private def addlog(msg: Log) = {
    DatabaseUtil.addLog(db, msg) onComplete {
      case Failure(e) ⇒ log.error(s"Failed to add log: ${e.getMessage}")
      case _ ⇒
    }
  }

  override def receive = {
    case Ready ⇒
      context become ready
      unstashAll()

    case _ ⇒ stash()
  }

  def ready: Receive = {
    case ar: Bot.ApiResponse ⇒
      ar.fields.getString("ok") match {
        case Some("true") ⇒
          ar.fields.get("members") match {
            case Some(jsa: JsArray) ⇒
              val users = jsa.elements.flatMap {
                case jso: JsObject ⇒ for {
                  id ← jso.fields.getString("id")
                  name ← jso.fields.getString("name")
                } yield {
                  User(None, id, name)
                }
                case _ ⇒ None
              }
              DatabaseUtil.updateUsers(db, users) onComplete {
                case Success(si) ⇒ log.info(s"Updated ${si.getOrElse(-1)} users")
                case Failure(t) ⇒ log.error(s"Error adding new users: ${t.getMessage}")
              }

            case _ ⇒ ar.fields.get("channels") match {
              case Some(jsa: JsArray) ⇒
                val channels = jsa.elements.flatMap {
                  case jso: JsObject ⇒ for {
                    id ← jso.fields.getString("id")
                    name ← jso.fields.getString("name")
                  } yield {
                    Channel(None, id, name)
                  }
                  case _ ⇒ None
                }
                DatabaseUtil.updateChannels(db, channels) onComplete {
                  case Success(si) ⇒ log.info(s"Updated ${si.getOrElse(-1)} channels")
                  case Failure(t) ⇒ log.error(s"Error adding new channels: ${t.getMessage}")
                }

              case _ ⇒ log.error("Got an ApiResponse that wasn't users or channels")
            }
          }
        case _ ⇒ log.error("Got an ApiResponse that wasn't ok")
      }

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
