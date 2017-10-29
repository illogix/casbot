package com.illojones.web.slack.cassie

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Stash}
import akka.pattern.ask
import com.illojones.web.slack.Bot.SendMessage
import com.illojones.web.slack.SlackEvents.RegularMessage
import com.illojones.web.slack.cassie.Cassie._
import com.illojones.web.slack.cassie.CassieMessages._
import com.illojones.web.slack.cassie.CassieUtils._
import com.illojones.web.slack.cassie.database.DatabaseActor
import com.illojones.web.slack.cassie.games.{Blackjack, GameActor, RPS}
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

  case class Game(id: Int, name: String, actor: ActorRef, originalChannel: String)

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

  def withdraw(user: String): String = {
    players.get(user) match {
      case Some(p) ⇒
        val newPlayer = p.copy(balance = p.balance + StartingBalance)
        updatePlayer(newPlayer).getOrElse(s"<@$user> now has ${newPlayer.balance}")
      case _ ⇒ Responses.unknown(user)
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

  private def printBalances(showPlayerBalance: Iterable[String]) = {
    val balances = showPlayerBalance.flatMap { user ⇒
      players.get(user).map(p ⇒ s"<@$user>: ${p.balance}")
    }
    s" New Balances: ${balances.mkString(", ")}"
  }

  private def sendMessage(msg: String, channel: Option[String] = None): Unit = {
    context.parent ! SendMessage(channel.getOrElse(defaultChannel), msg)
  }

  private def sendHelpMessage(channel: String, sub: String): Unit = {
    sub.toLowerCase match {
      case "rps" ⇒ sendMessage(HelpMessages.RPS, Some(channel))
      case "bj" ⇒ sendMessage(HelpMessages.Blackjack, Some(channel))
      case _ ⇒ sendMessage(HelpMessages.General, Some(channel))
    }
  }

  private def addPlayer(user: String) = {
    val p = Player(user)
    updatePlayer(p) foreach { sendMessage(_) }
    p
  }

  private def launchGame(game: String, user: String, channel: String, props: (Int) ⇒ Props) = {
    val player = players.getOrElse(user, addPlayer(user))
    val id = getTableId()
    val actor = context.actorOf(props(id), id.toString)
    games = games + (id → Game(id, game, actor, channel))
    updatePlayer(player.copy(currentTable = Some(id)), writeToDb = false)
  }

  private def newGame(user: String, channel: String, game: String, ante: Option[String]) = {
    game match {
      case "rps" ⇒ launchGame("rps", user, channel, (id) ⇒ RPS.props(RPS.RPSInitTable(id, user, betInt(ante))))
      case "bj" ⇒ launchGame("bj", user, channel, (id) ⇒ Blackjack.props(Blackjack.BJInitTable(id, user, ante.map(betInt))))
      case _ ⇒ sendMessage(s"Game '$game' doesn't exist!", Some(channel))
    }
  }

  private def joinGame(user: String, table: String, channel: String): Unit = {
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

  private def canPlayerCover(user: String, bet: Int, alert: Boolean = true) = {
    players.get(user).map(_.balance) match {
      case Some(bal) ⇒
        val canCover = bal >= bet
        if (alert && !canCover) sendMessage(Responses.cantCover(user, bet, bal))
        canCover
      case None ⇒
        sendMessage(Responses.unknown(user))
        false
    }
  }

  def normal: Receive = {
    case RegularMessage(channel, user, text, ts, sourceTeam, team) ⇒
      text match {
        case GameCommands.Help(sub) ⇒ sendHelpMessage(channel, sub)

        case GameCommands.TableCommand(id, msg) ⇒
          players.get(user).flatMap(_.currentTable) match {
            case Some(table) if table.toString == id && games.contains(table) ⇒
              games.get(table).map(_.actor).foreach { _ ! TableMessage(user, msg) }
            case _ ⇒ sendMessage(Responses.Shrug, Some(channel))
          }

        case GameCommands.SitWithAnte(game, ante) if !isSitting(user, Some(channel)) && canPlayerCover(user, betInt(ante)) ⇒
          newGame(user, channel, game, Some(ante))

        case GameCommands.SitWithoutAnte(game) if !isSitting(user, Some(channel)) ⇒
          newGame(user, channel, game, None)

        case GameCommands.JoinTable(table) if !isSitting(user, Some(channel)) ⇒
          joinGame(user, table, channel)

        case GameCommands.LeaveTable ⇒
          // TODO - outstanding hands, or just subtract money @ bet
          val player = players.get(user)
          player.flatMap(_.currentTable).flatMap(games.get).map(_.actor).foreach { _ ! LeaveGame(user) }
          player.foreach(p ⇒ updatePlayer(p.copy(currentTable = None), writeToDb = false))

        case GameCommands.Withdraw ⇒
          players.get(user) match {
            case Some(p) ⇒ p.currentTable match {
              case Some(_) ⇒ sendMessage("Must leave table to withdraw.")
              case None ⇒ sendMessage(withdraw(user))
            }
            case None ⇒
              addPlayer(user)
              sendMessage(withdraw(user))
          }

        case GameCommands.SoloRoshambo(ante, play) if !isSitting(user, Some(channel)) && canPlayerCover(user, betInt(ante)) ⇒
          launchGame("rps", user, channel, (id) ⇒ RPS.props(RPS.RPSInitSolo(id, user, betInt(ante), play)))

        case GameCommands.SoloBlackjack(ante) if !isSitting(user, Some(channel)) && canPlayerCover(user, betInt(ante)) ⇒
          launchGame("bj", user, channel, (id) ⇒ Blackjack.props(Blackjack.BJInitSolo(id, user, betInt(ante))))

        case GameCommands.TestUser(username, cmd) ⇒ self ! RegularMessage(channel, username, cmd, ts, sourceTeam, team)

        case _ ⇒
      }

    case sm: SendCassieMessage ⇒
      val actor = sender()
      games.find(_._2.actor == actor) match {
        case Some((id, game)) ⇒
          val channel = if (sm.originalChannel) Some(game.originalChannel) else None
          sendMessage(s"[$id] ${sm.message}", channel)
        case None ⇒
          log.error(s"Can't find game for $sm")
      }

    case GameActor.Terminated ⇒
      val actor = sender()
      context.stop(actor)
      games.find(_._2.actor == actor) match {
        case Some((id, _)) ⇒
          games = games - id
          val errs = players.filter(_._2.currentTable.contains(id)).flatMap { p ⇒
            updatePlayer(p._2.copy(currentTable = None), writeToDb = false)
          }
          if (errs.nonEmpty)
            sendMessage(s"Errors terminating table $id: ${errs.mkString(", ")}")
        case None ⇒ log.error("Got Terminated from unknown actor")
      }

    case Result(balanceUpdates, msg) ⇒
      val actor = sender()
      val tableString = games.find(_._2.actor == actor) match {
        case Some((id, _)) ⇒ s"[${id.toString}] "
        case None ⇒ ""
      }

      balanceUpdates.foreach {
        case (user, diff) if diff != 0 ⇒
          players.get(user).foreach { p ⇒ updatePlayer(p.copy(balance = p.balance + diff)) }
        case _ ⇒
      }

      sendMessage(tableString + msg + printBalances(balanceUpdates.keys), None)

    case CanPlayerCoverRequest(user, bet, alert) ⇒
      sender() ! CanPlayerCoverResponse(user, canPlayerCover(user, bet, alert))

    case _ ⇒
  }
}
