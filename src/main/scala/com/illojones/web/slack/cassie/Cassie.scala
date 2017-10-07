package com.illojones.web.slack.cassie

import akka.actor.{Actor, ActorLogging, PoisonPill, Props, Stash}
import akka.pattern.ask
import com.illojones.web.slack.Bot.SendMessage
import com.illojones.web.slack.SlackMessages
import com.illojones.web.slack.cassie.Cassie._
import com.illojones.web.slack.cassie.database.DatabaseActor
import com.illojones.web.slack.cassie.games.RPS
import com.typesafe.config.Config

import scala.concurrent.Await
import scala.concurrent.duration._

object Cassie {
  def props(config: Config) = Props(new Cassie(config))
  case object Init

  final val StartingBalance = 1000
  case class Player(user: String, balance: Int = StartingBalance, currentGame: Option[Game] = None)

  sealed trait Game {
    def ante: Int
  }

  case class Blackjack(ante: Int) extends Game

  case class Roshambo(ante: Int) extends Game

  object GameCommands {
    final val Withdraw = "!atm"
    final val Blackjack = "!bj (\\d+)".r
    final val Roshambo = "!rps (\\d+) (rock|paper|scissors|r|p|s)".r
  }

  object BlackjackCommands {
    final val Hit = "!hit".r
    final val Stay = "!stay".r
  }

  final val Shrug = "¯\\_(ツ)_/¯"

  trait Result {
    def winnings(ante: Int): Int
    def message: String
  }

}

class Cassie(config: Config) extends Actor with ActorLogging with Stash {

  private val dbActor = context.actorOf(DatabaseActor.props(config), "dbActor")

  private var players: Map[String, Player] = Map.empty

  override def preStart(): Unit = {
    super.preStart()

    self ! Init
  }

  def updatePlayer(updatedPlayer: Player) = {
    implicit val timeout: akka.util.Timeout = 5.seconds
    val respF = (dbActor ? DatabaseActor.UpdatePlayer(updatedPlayer)).mapTo[DatabaseActor.UpdatePlayerResponse]
    val resp = Await.result(respF, 5.seconds)
    if (resp.errorMsg.isEmpty) players = players + (updatedPlayer.user → updatedPlayer)

    resp.errorMsg
  }

  def add(user: String, game: Game, result: Option[Result] = None) = {
    val player = players.getOrElse(user, Player(user))
    player.currentGame match {
      case Some(cg) ⇒ s"<@$user> is already playing ${cg.getClass.getSimpleName}"
      case _ if game.ante <= player.balance ⇒
        result match {
          case None ⇒
            val newPlayer = player.copy(balance = player.balance - game.ante, currentGame = Some(game))
            updatePlayer(newPlayer).getOrElse(s"<@$user> is now playing ${game.getClass.getSimpleName}")
          case Some(res) ⇒
            val newBalance = player.balance - game.ante + res.winnings(game.ante)
            val updatedPlayer = player.copy(balance = newBalance, currentGame = None)
            updatePlayer(updatedPlayer).getOrElse(s"${res.message} (new balance: $newBalance)")
        }
      case _ ⇒
        s"<@$user> doesn't have ${game.ante} (${player.balance})"
    }
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

  def normal: Receive = {
    case SlackMessages.CassieMessage(user, channel, text) ⇒
      log.info(s"CassieMessage($user, $channel, $text)")
      val response = text match {
        case GameCommands.Withdraw ⇒ Some(withdraw(user))
        case GameCommands.Blackjack(ante) ⇒ Some(add(user, Blackjack(ante.toInt)))
        case GameCommands.Roshambo(ante, play) ⇒ Some(add(user, Roshambo(ante.toInt), Some(RPS.result(play, user))))
        case gameText ⇒ players.get(user) match {
          case Some(p) ⇒ p.currentGame match {
            case Some(g: Blackjack) ⇒ gameText match {
              case BlackjackCommands.Hit() ⇒ Some(s"hit")
              case BlackjackCommands.Stay() ⇒ Some(s"stay")
              case _ ⇒ None
            }
            case _ ⇒ None
          }
          case _ ⇒ None
        }
      }
      response foreach (resp ⇒ sender() ! SendMessage(channel, resp))

    case _ ⇒
  }
}
