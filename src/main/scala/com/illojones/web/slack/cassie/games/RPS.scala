package com.illojones.web.slack.cassie.games

import com.illojones.web.slack.cassie.Cassie.Result

import scala.util.Random

object RPS {
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

  case class Win(user: String, player: Move, dealer: Move) extends Result {
    def winnings(ante: Int): Int = 2 * ante
    def message = s"<@$user>: ${player.toString}, Dealer: ${dealer.toString}. You win!"
  }

  case class Lose(user: String, player: Move, dealer: Move) extends Result {
    def winnings(ante: Int): Int = 0
    def message = s"<@$user>: ${player.toString}, Dealer: ${dealer.toString}. You lose."
  }

  case class Draw(user: String, player: Move, dealer: Move) extends Result {
    def winnings(ante: Int): Int = ante
    def message = s"<@$user>: ${player.toString}, Dealer: ${dealer.toString}. Draw."
  }

  def result(play: String, user: String): Result = {
    val housePlay = Random.nextInt(3) match {
      case 0 ⇒ Rock
      case 1 ⇒ Paper
      case 2 ⇒ Scissors
    }

    val playerPlay = play match {
      case "r" | "rock" ⇒ Rock
      case "p" | "paper" ⇒ Paper
      case "s" | "scissors" ⇒ Scissors
    }

    (playerPlay, housePlay) match {
      case (p, h) if p == h ⇒ Draw(user, playerPlay, housePlay)
      case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) ⇒ Win(user, playerPlay, housePlay)
      case _ ⇒ Lose(user, playerPlay, housePlay)
    }
  }


}
