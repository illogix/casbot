package com.illojones.web.slack.cassie

import com.illojones.web.slack.cassie.Cassie.{Game, Player}

object CassieMessages {

  /** General **/

  object GameCommands {
    final val Help = "!help\\s*(.*)".r
    final val Withdraw = "!atm"
    final val SoloRoshambo = "!rps (\\d+) (rock|paper|scissors|r|p|s)".r
    final val SoloBlackjack = "!bj (\\d+)".r

    final val TableCommand = "!(\\d+) (.*)".r

    final val Ante = "bet (\\d+)".r

    final val SitWithAnte = "!sit (rps|bj) (\\d+)".r
    final val SitWithoutAnte = "!sit (bj)".r
    final val JoinTable = "!sit (\\d+)".r
    final val LeaveTable = "!leave"

    final val TestUser = "!testuser (\\w+) (.*)".r
  }

  object Responses {
    final val Shrug = "¯\\_(ツ)_/¯"
    def unknown(user: String) = s"Unknown player <@$user>"

    def alreadySitting(user: String, game: Option[Game]) =
      s"<@$user> is already sitting at ${game.map(g ⇒ s"${g.name}, table ${g.id}").getOrElse("an unknown table!")}"

    def cantCover(user: String, bet: Int, balance: Int) = s"<@$user> can't cover $bet (balance=$balance)"
  }

  /** Blackjack **/

  object BlackjackCommands {
    final val Hit = "hit"
    final val Stand = "stand|stay".r
  }

  object BlackjackResponses {
    final val EmptyDeck = "Deck is out of cards! Exiting..."
    final val NoPlayers = "Nobody playing, closing table"
    final val NewGameStarting = "New blackjack hand starting in 10 seconds"
    final val FullTable = "Sorry, table is full."
    def sat(user: String, seatNum: Int) = s"<@$user> sat at Blackjack in seat #$seatNum"
  }

  case class JoinGame(user: String)
  case class LeaveGame(user: String)
  case class UpdatePlayer(updatedPlayer: Player)
  case class TableMessage(user: String, msg: String)

  case class CanPlayerCoverRequest(user: String, bet: Int, alert: Boolean = true)
  case class CanPlayerCoverResponse(user: String, canCover: Boolean)

  case class SendCassieMessage(message: String, originalChannel: Boolean = false)

  /** Help **/

  object HelpMessages {
    final val General =
      """Top-level Commands:
        |`!atm`: Withdraw 1000
        |`!sit <game> [ante]`: Start new game. Initial ante is required for _rps_, optional for _bj_
        |`!sit <table#>`: Join existing table
        |`!help [bj|rps]`: Help [for game]
        |`!leave`: Leave current table
        |`!<table#> <msg>`: Game-specific command
      """.stripMargin

    final val RPS =
      """RPS Help:
        |`!rps <ante> r|rock|p|paper|s|scissors`: Quick play (1 game vs Dealer)
        |`!<table#> r|rock|p|paper|s|scissors`: Throw rock/paper/scissors
      """.stripMargin

    final val Blackjack =
      """Blackjack Help:
        |`!bj <ante>`: Quick play (1 game vs Dealer)
        |`!<table#> hit|stand|stay`: Do the thing
        |`!<table#> bet [bet]`: Ante before each hand
      """.stripMargin
  }
}
