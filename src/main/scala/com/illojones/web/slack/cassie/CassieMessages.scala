package com.illojones.web.slack.cassie

import com.illojones.web.slack.cassie.Cassie.{Game, Player}

object CassieMessages {
  object GameCommands {
    final val Withdraw = "!atm"
    final val Blackjack = "!bj (\\d+)".r
    final val SoloRoshambo = "!rps (\\d+) (rock|paper|scissors|r|p|s)".r

    final val TableCommand = "!(\\d+) (.*)".r

    final val SitGame = "!sit (rps) (\\d+)".r
    final val SitTable = "!sit (\\d+)".r
    final val LeaveTable = "!leave"
  }

  object Responses {
    val Shrug = "¯\\_(ツ)_/¯"

    def alreadySitting(user: String, game: Option[Game]) =
      s"<@$user> is already sitting at ${game.map(g ⇒ s"${g.name}, table ${g.id}").getOrElse("an unknown table!")}"
  }

  case class JoinGame(user: String)
  case class LeaveGame(user: String)
  case class UpdatePlayer(updatedPlayer: Player)
  case class TableMessage(user: String, msg: String)

  case class SendCassieMessage(message: String, channel: Option[String] = None)
}
