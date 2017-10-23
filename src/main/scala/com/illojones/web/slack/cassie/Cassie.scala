package com.illojones.web.slack.cassie

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Stash}
import akka.pattern.ask
import com.illojones.web.slack.Bot.SendMessage
import com.illojones.web.slack.SlackEvents.RegularMessage
import com.illojones.web.slack.cassie.Cassie._
import com.illojones.web.slack.cassie.CassieMessages._
import com.illojones.web.slack.cassie.database.DatabaseActor
import com.illojones.web.slack.cassie.games.{GameActor, RPS}
import com.typesafe.config.Config

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object Cassie {
  def props(config: Config) = Props(new Cassie(config))
  case object Init

  final val StartingBalance = 1000
  case class Player(user: String, balance: Int = StartingBalance, currentTable: Option[Int] = None)

  case class Result(playerResults: Map[String, Int], message: String)

  case class Game(id: Int, name: String, actor: ActorRef)

}

class Cassie(config: Config) extends Actor with ActorLogging with Stash {

  private val dbActor = context.actorOf(DatabaseActor.props(config), "dbActor")

  private var players: Map[String, Player] = Map.empty
  private var games: Map[Int, Game] = Map.empty
  private var defaultChannel: String = ""

  @tailrec
  private def getTableId(check: Int = 1): Int = if (games.contains(check)) getTableId(check + 1) else check

  override def preStart(): Unit = {
    super.preStart()

    Try(config.getString("cassie.casChannel")) match {
      case Success(c) ⇒
        defaultChannel = c
        self ! Init
      case Failure(t) ⇒
        log.error(s"Couldn't get config cassie.casChannel: ${t.getMessage}")
        self ! PoisonPill
    }
  }

  private def updatePlayer(updatedPlayer: Player, writeToDb: Boolean = true) = {
    val error = if (writeToDb) {
      implicit val timeout: akka.util.Timeout = 5.seconds
      val respF = (dbActor ? DatabaseActor.UpdatePlayer(updatedPlayer)).mapTo[DatabaseActor.UpdatePlayerResponse]
      val resp = Await.result(respF, 5.seconds)
      resp.errorMsg
    } else None
    if (error.isEmpty) players = players + (updatedPlayer.user → updatedPlayer)

    error
  }

  def withdraw(user: String) = {
    players.get(user) match {
      case Some(p) ⇒
        val newPlayer = p.copy(balance = p.balance + StartingBalance)
        updatePlayer(newPlayer).getOrElse(s"<@$user> now has ${newPlayer.balance}")
      case _ ⇒ s"i don't know you brah"
    }
  }

  override def receive: Receive = {
    case Init ⇒ dbActor ! DatabaseActor.GetPlayers

    case DatabaseActor.GetPlayersResponse(allPlayers, errorMsg) ⇒
      errorMsg match {
        case Some(err) ⇒
          log.error(err)
          self ! PoisonPill
        case _ ⇒
          log.info(s"Got players: $allPlayers")
          players = allPlayers.map(p ⇒ p.user → p).toMap
          context become normal
          unstashAll()
      }

    case _ ⇒ stash()
  }


  private def sendMessage(msg: String, channel: Option[String] = None) = {
    context.parent ! SendMessage(channel.getOrElse(defaultChannel), msg)
  }

  private def addPlayer(user: String) = {
    val p = Player(user)
    updatePlayer(p) foreach { sendMessage(_) }
    p
  }

  private def newGame(user: String, game: String, ante: String, channel: String) = {
    val player = players.getOrElse(user, addPlayer(user))

    game match {
      case "rps" ⇒
        val id = getTableId()
        Try(ante.toInt).toOption.foreach { anteInt ⇒
          val actor = context.actorOf(RPS.props(RPS.RPSInitTable(user, anteInt)), id.toString)
          games = games + (id → Game(id, "rps", actor))
          updatePlayer(player.copy(currentTable = Some(id)), writeToDb = false)
        }
      case _ ⇒
        sendMessage(s"Game '$game' doesn't exist!", Some(channel))
    }
  }

  private def joinGame(user: String, table: String, channel: String) = {
    val player = players.getOrElse(user, addPlayer(user))

    Try(table.toInt).toOption.flatMap(games.get) match {
      case Some(cg) ⇒
        updatePlayer(player.copy(currentTable = Some(cg.id)), writeToDb = false)
        cg.actor ! JoinGame(player.user)
      case _ ⇒ context.parent ! sendMessage(s"Table $table doesn't exist!", Some(channel))
    }
  }

  private def isSitting(user: String, announceChannel: Option[String] = None, alert: Boolean = true): Boolean = {
    players.get(user).flatMap(_.currentTable) match {
      case Some(ct) ⇒
        if (alert) sendMessage(Responses.alreadySitting(user, games.get(ct)), announceChannel)
        true
      case None ⇒ false
    }
  }

  def normal: Receive = {
    case RegularMessage(channel, user, text, ts, sourceTeam, team) ⇒
      log.debug(s"RegularMessage($channel, $user, $text, $ts, $sourceTeam, $team)")
      text match {
        case GameCommands.TableCommand(id, msg) ⇒
          players.get(user).flatMap(_.currentTable) match {
            case Some(table) if table.toString == id && games.contains(table) ⇒
              games.get(table).map(_.actor).foreach { _ ! TableMessage(user, msg) }
            case _ ⇒ sendMessage(Responses.Shrug, Some(channel))
          }

        case GameCommands.SitGame(game, ante) ⇒
          if (!isSitting(user, Some(channel))) newGame(user, game, ante, channel)

        case GameCommands.SitTable(table) ⇒
          if (!isSitting(user, Some(channel))) joinGame(user, table, channel)

        case GameCommands.LeaveTable ⇒
          val player = players.get(user)
          player.flatMap(_.currentTable).flatMap(games.get).map(_.actor).foreach { _ ! LeaveGame(user) }
          player.foreach(p ⇒ updatePlayer(p.copy(currentTable = None), writeToDb = false))

        case GameCommands.Withdraw ⇒ sendMessage(withdraw(user))

        case GameCommands.SoloRoshambo(ante, play) ⇒
          Try(ante.toInt).toOption.foreach { a ⇒
            val id = getTableId()
            val actor = context.actorOf(RPS.props(RPS.RPSInitSolo(user, a, play)), id.toString)
            games = games + (id → Game(id, "rps", actor))
          }

        case _ ⇒
      }

    case sm: SendCassieMessage ⇒
      val actor = sender()
      val tableId = games.find(_._2.actor == actor) match {
        case Some((id, _)) ⇒ id.toString
        case None ⇒ Responses.Shrug
      }

      sendMessage(s"[$tableId] ${sm.message}", sm.channel)

    case GameActor.Terminated ⇒
      val actor = sender()
      context.stop(actor)
      games.find(_._2.actor == actor) match {
        case Some((id, _)) ⇒ games = games - id
        case None ⇒ log.error("Got Terminated from unknown actor")
      }

    case Result(balanceUpdates, msg) ⇒
      val actor = sender()
      val tableString = games.find(_._2.actor == actor) match {
        case Some((id, _)) ⇒ s"[${id.toString}] "
        case None ⇒ ""
      }

      balanceUpdates.foreach { case (user, diff) ⇒
        players.get(user).foreach { p ⇒ updatePlayer(p.copy(balance = p.balance + diff)) }
      }

      sendMessage(tableString + msg)

    case _ ⇒
  }
}
