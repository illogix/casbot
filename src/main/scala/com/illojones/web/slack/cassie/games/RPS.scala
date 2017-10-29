package com.illojones.web.slack.cassie.games

import akka.actor.Props
import com.illojones.web.slack.Bot.SendMessage
import com.illojones.web.slack.cassie.Cassie.Result
import com.illojones.web.slack.cassie.CassieMessages._
import com.illojones.web.slack.cassie.games.GameActor._
import com.illojones.web.slack.cassie.games.RPS._

import scala.util.Random

object RPS {
  def props(init: InitMessage) = Props(new RPS(init))

  case class RPSInitSolo(id: Int, player: String, ante: Int, move: String) extends SoloInitMessage
  case class RPSInitTable(id: Int, player: String, ante: Int) extends TableInitMessage

//  case class RPSState() extends GameState
//  case class RPSPlayerState() extends PlayerState

  sealed trait Move
  case object Rock extends Move {
    override def toString: String = ":fist:"
  }
  case object Paper extends Move {
    override def toString: String = ":hand:"
  }
  case object Scissors extends Move {
    override def toString: String = ":v:"
  }
  case object NoMove extends Move

  private def randomMove = Random.nextInt(3) match {
    case 0 ⇒ Rock
    case 1 ⇒ Paper
    case 2 ⇒ Scissors
  }

  private def getMoveFromMessage(msg: String) = msg.toLowerCase match {
    case "r" | "rock" ⇒ Rock
    case "p" | "paper" ⇒ Paper
    case "s" | "scissors" ⇒ Scissors
    case _ ⇒ NoMove
  }

  private[RPS] def getResult(ante: Int, player: String, playerMove: Move, p2: Option[(String, Move)] = None): Result = {
    val p2Name = p2.map(id ⇒ s"<@${id._1}>").getOrElse("Dealer")
    val p2Move = p2.map(_._2).getOrElse(randomMove)

    val playString = s"<@$player>: ${playerMove.toString}, $p2Name: ${p2Move.toString}."

    (playerMove, p2Move) match {
      case (_, NoMove) | (NoMove, _) ⇒ Result(Map.empty, "hax!")
      case (p, h) if p == h ⇒
        val res = Seq(player → 0) ++ p2.map(_._1 → ante)
        Result(res.toMap, s"$playString Draw.")
      case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) ⇒
        val res = Seq(player → ante) ++ p2.map(_._1 → (0 - ante))
        Result(res.toMap, s"$playString <@$player> wins!")
      case _ ⇒
        val res = Seq(player → (0 - ante)) ++ p2.map(_._1 → ante)
        Result(res.toMap, s"$playString $p2Name wins!")
    }
  }

}

class RPS(init: InitMessage) extends GameActor {

  override def preStart(): Unit = {
    super.preStart()

    self ! init
  }

  def waitingP2(ante: Int, p1: String): Receive = {
    case JoinGame(user) ⇒
      context.parent ! SendCassieMessage(s"<@$user> joined RPS. Game on!")
      context become table(ante, p1, user, None, None)
    case LeaveGame(user) ⇒
      context.parent ! SendCassieMessage(s"<@$user> left RPS. Table closed.")
      context.parent ! Terminated
  }

  private def tableResult(ante: Int, p1: String, p1m: Move, p2: String, p2m: Move) = {
    context.parent ! getResult(ante, p1, p1m, Some((p2, p2m)))
    context become table(ante, p1, p2, None, None)
  }

  def table(ante: Int, p1: String, p2: String, p1Move: Option[Move], p2Move: Option[Move]): Receive = {
    case TableMessage(user, msg) if p1 == user || p2 == user ⇒
      getMoveFromMessage(msg) match {
        case NoMove ⇒ context.parent ! SendCassieMessage(Responses.Shrug)

        case p1m if user == p1 ⇒
          p2Move match {
            case Some(p2m) ⇒ tableResult(ante, p1, p1m, p2, p2m)
            case None ⇒ context become table(ante, p1, p2, Some(p1m), p2Move)
          }

        case p2m if user == p2 ⇒
          p1Move match {
            case Some(p1m) ⇒ tableResult(ante, p1, p1m, p2, p2m)
            case None ⇒ context become table(ante, p1, p2, p1Move, Some(p2m))
          }
      }

    case LeaveGame(user) if p1 == user || p2 == user ⇒
      context.parent ! SendCassieMessage(s"<@$user> left RPS. Awaiting another player.")
      context become waitingP2(ante, if (p1 == user) p2 else p1)
  }

  override def receive = {
    case RPSInitSolo(_, player, ante, move) ⇒
      context.parent ! getResult(ante, player, getMoveFromMessage(move))
      context.parent ! Terminated

    case RPSInitTable(_, player, ante) ⇒
      context.parent ! SendCassieMessage(s"<@$player> sat at RPS with a bet of $ante.  Waiting for an opponent...")
      context become waitingP2(ante, player)
  }
}
