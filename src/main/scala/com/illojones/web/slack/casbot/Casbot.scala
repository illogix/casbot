package com.illojones.web.slack.casbot

import akka.actor.{Actor, ActorLogging, PoisonPill, Props, Stash}
import akka.pattern.ask
import com.illojones.web.slack.SlackMessages
import com.illojones.web.slack.casbot.Casbot._
import com.illojones.web.slack.casbot.database.DatabaseActor
import com.illojones.web.slack.casbot.games.RPS

import scala.concurrent.Await
import scala.concurrent.duration._

object Casbot {
  case object Init

  final val StartingBalance = 1000
  case class Player(id: String, name: String, balance: Int = StartingBalance, currentGame: Option[Game] = None)

  sealed trait Game {
    def ante: Int
  }

  case class Blackjack(ante: Int) extends Game

  case class Roshambo(ante: Int) extends Game

  object GameCommands {
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

class Casbot extends Actor with ActorLogging with Stash {

  private val dbActor = context.actorOf(Props[DatabaseActor], "dbActor")

  private var players: Map[String, Player] = Map.empty

  override def preStart(): Unit = {
    super.preStart()

    self ! Init
  }

  def updatePlayer(updatedPlayer: Player) = {
    implicit val timeout: akka.util.Timeout = 5.seconds
    val respF = (dbActor ? DatabaseActor.UpdatePlayer(updatedPlayer)).mapTo[DatabaseActor.UpdatePlayerResponse]
    val resp = Await.result(respF, 5.seconds)
    if (resp.errorMsg.isEmpty) players = players + (updatedPlayer.id → updatedPlayer)

    resp.errorMsg
  }

  def add(id: String, name: String, game: Game, result: Option[Result] = None) = {
    val player = players.getOrElse(id, Player(id, name))
    player.currentGame match {
      case Some(cg) ⇒ s"$name is already playing ${cg.getClass.getSimpleName}"
      case _ if game.ante <= player.balance ⇒
        result match {
          case None ⇒
            val newPlayer = player.copy(balance = player.balance - game.ante, currentGame = Some(game))
            updatePlayer(newPlayer).getOrElse(s"$name is now playing ${game.getClass.getSimpleName}")
          case Some(res) ⇒
            val newBalance = player.balance - game.ante + res.winnings(game.ante)
            val updatedPlayer = player.copy(balance = newBalance, currentGame = None)
            updatePlayer(updatedPlayer).getOrElse(s"${res.message} (new balance: $newBalance)")
        }
      case _ ⇒
        s"$name doesn't have ${game.ante} (${player.balance})"
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
          players = allPlayers.map(p ⇒ p.id → p).toMap
          context become normal
          unstashAll()
      }

    case _ ⇒ stash()
  }

  def normal: Receive = {
    case im: SlackMessages.IncomingMessage ⇒
      log.info(s"IncomingMessage($im)")
      val response = im.text match {
        case GameCommands.Blackjack(ante) ⇒ add(im.user_id, im.user_name, Blackjack(ante.toInt))
        case GameCommands.Roshambo(ante, play) ⇒ add(im.user_id, im.user_name, Roshambo(ante.toInt), Some(RPS.result(play, im.user_name)))
        case text ⇒ players.get(im.user_id) match {
          case Some(p) ⇒ p.currentGame match {
            case Some(g: Blackjack) ⇒ text match {
              case BlackjackCommands.Hit() ⇒ s"hit"
              case BlackjackCommands.Stay() ⇒ s"stay"
              case _ ⇒ Shrug
            }
            case _ ⇒ Shrug
          }
          case _ ⇒ Shrug
        }


      }
      sender() ! SlackMessages.Response(response)

    case _ ⇒
  }
}
