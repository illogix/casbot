package com.illojones.web.slack.casbot.games

import com.illojones.web.slack.casbot.Casbot.Result

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

  case class Win(playerName: String, player: Move, dealer: Move) extends Result {
    def winnings(ante: Int): Int = 2 * ante
    def message = s"$playerName: ${player.toString}, Dealer: ${dealer.toString}. You win!"
  }

  case class Lose(playerName: String, player: Move, dealer: Move) extends Result {
    def winnings(ante: Int): Int = 0
    def message = s"$playerName: ${player.toString}, Dealer: ${dealer.toString}. You lose."
  }

  case class Draw(playerName: String, player: Move, dealer: Move) extends Result {
    def winnings(ante: Int): Int = ante
    def message = s"$playerName: ${player.toString}, Dealer: ${dealer.toString}. Draw."
  }

  def result(play: String, playerName: String): Result = {
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
      case (p, h) if p == h ⇒ Draw(playerName, playerPlay, housePlay)
      case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) ⇒ Win(playerName, playerPlay, housePlay)
      case _ ⇒ Lose(playerName, playerPlay, housePlay)
    }
  }


}
