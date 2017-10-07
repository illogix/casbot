package com.illojones.web.slack.cassie.database

import akka.actor.{Actor, ActorLogging, PoisonPill, Stash}
import com.illojones.web.slack.cassie.Cassie.Player
import com.illojones.web.slack.cassie.database.DatabaseActor._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object DatabaseActor {
  case object Ready

  sealed trait Request
  case class UpdatePlayer(p: Player) extends Request
  case object GetPlayers extends Request

  sealed trait Response {
    def errorMsg: Option[String]
  }
  case class UpdatePlayerResponse(errorMsg: Option[String] = None) extends Response
  case class GetPlayersResponse(players: Seq[Player], errorMsg: Option[String] = None) extends Response
}

class DatabaseActor extends Actor with ActorLogging with Stash {
  val db = DatabaseUtil.getDatabase

  override def preStart(): Unit = {
    super.preStart()

    DatabaseUtil.createTableIfNeeded(db).onComplete {
      case Success(_) ⇒ self ! Ready
      case Failure(_) ⇒
        log.error("Failed to create table!")
        self ! PoisonPill
    }
  }

  def receive = {
    case Ready ⇒
      context become ready
      unstashAll()

    case _ ⇒ stash()
  }

  def ready: Receive = {
    case UpdatePlayer(p: Player) ⇒
      val replyTo = sender()

      DatabaseUtil.updatePlayer(db, p) onComplete {
        case Success(_) ⇒ replyTo ! UpdatePlayerResponse()
        case Failure(e) ⇒ replyTo ! UpdatePlayerResponse(Some(e.getMessage))
      }

    case GetPlayers ⇒
      val replyTo = sender()

      DatabaseUtil.getPlayers(db) onComplete {
        case Success(s) ⇒ replyTo ! GetPlayersResponse(s)
        case Failure(e) ⇒ replyTo ! GetPlayersResponse(Seq.empty, Some(e.getMessage))
      }

    case msg ⇒ log.error(s"DatabaseActor got unexpected message $msg")
  }
}
