package com.illojones.web.slack.cassie.games

import akka.actor.{Props, Stash}
import akka.pattern.ask
import akka.util.Timeout
import com.illojones.web.slack.cassie.Cassie.Result
import com.illojones.web.slack.cassie.{Card, Deck}
import com.illojones.web.slack.cassie.CassieMessages._
import com.illojones.web.slack.cassie.CassieUtils._
import com.illojones.web.slack.cassie.games.Blackjack._
import com.illojones.web.slack.cassie.games.GameActor._

import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

object Blackjack {
  def props(init: InitMessage) = Props(new Blackjack(init))

  case class BJInitTable(id: Int, user: String, ante: Option[Int]) extends TableInitMessage
  case class BJInitSolo(id: Int, user: String, ante: Int) extends TableInitMessage

  case object NewGameCountdown
  case object NewGameReady
  case object NewGameStart
  case object DealerTurn

  case class PlayerSeat(user: String, bet: Option[Int], hand: Option[Hand] = None) {
    val isActive: Boolean = bet.nonEmpty

    def reset = PlayerSeat(user, None)
  }

  case class Hand(cards: Seq[Card] = Seq.empty) {
    private final val HiddenCard = ":black_joker:"
    private lazy val isSoft: Boolean = cards.exists(_.rank == 1)

    private def cardValue(card: Card) = card.rank match {
      case 11 | 12 | 13 ⇒ 10
      case n ⇒ n
    }

    def addCard(card: Card): Hand = this.copy(cards = cards :+ card)

    private lazy val handValues: Seq[Int] = {
      val handValue = cards.map(cardValue).sum

      if (isSoft && handValue <= 11) Seq(handValue, handValue + 10) else Seq(handValue)
    }

    lazy val softSum: Int = handValues.head
    lazy val hardSum: Int = handValues.last

    def isSplittable: Boolean = cards.length == 2 && cards.head.rank == cards(1).rank
    def isDoubleable: Boolean = cards.length == 2 && hardSum == 11
    def isBust: Boolean = softSum > 21
    def is21: Boolean = hardSum == 21
    def isBlackjack: Boolean = cards.length == 2 && is21

    def playerCanHit: Boolean = !is21 && !isBust

    def dealerMustHit: Boolean = hardSum < 17 || (isSoft && softSum == 17)

    lazy val printSum = if (isBlackjack) "BJ" else hardSum
    def print(dealer: Boolean = false, end: Boolean = false) = {
      if (end) {
        s"${cards.mkString(" ")} (${if (hardSum > 21) "bust" else printSum})"
      } else if (dealer) {
        s"${cards.head} $HiddenCard"
      } else {
        // TODO - don't print soft value if hand is done
        s"${cards.mkString(" ")} (${if (hardSum > 21) "bust" else handValues.mkString("/")})"
      }
    }
  }
}

class Blackjack(init: InitMessage) extends GameActor with Stash {
  override def preStart(): Unit = {
    super.preStart()

    self ! init
  }

  private def printHands(players: Seq[PlayerSeat], dealer: Hand, end: Boolean) = {
    val dealerHand = s"Dealer: ${dealer.print(dealer = true, end)}"
    val playerHands = for {
      p ← players
      hand ← p.hand
    } yield {
      s"<@${p.user}>: ${hand.print(dealer = false, end)}"
    }
    s"$dealerHand, ${playerHands.mkString(", ")}."
  }

  private def printAction(player: Option[PlayerSeat]) = {
    player.map(p ⇒ s" <@${p.user}>'s turn.").getOrElse("")
  }

  @tailrec
  private def getNextActionSeat(seats: Seq[PlayerSeat], actionSeat: Int): Option[Int] = {
    seats.lift(actionSeat) match {
      case Some(s) if s.hand.exists(_.playerCanHit) ⇒ Some(actionSeat)
      case Some(_) ⇒ getNextActionSeat(seats, actionSeat + 1)
      case _ ⇒ None
    }
  }

  private def getResult(seat: PlayerSeat, dealer: Hand): Option[(String, Int)] = {
    seat.hand.flatMap { h ⇒
      val bet = seat.bet.getOrElse(0)
      val winnings = {
        if (dealer.isBust) {
          bet
        } else {
          if (h.isBlackjack) {
            math.ceil(bet * 3.0 / 2).toInt
          } else if (h.isBust || dealer.hardSum > h.hardSum) {
            0 - bet
          } else if (dealer.hardSum == h.hardSum) {
            0
          } else {
            bet
          }
        }
      }
      Some(seat.user, winnings)
    }
  }

  def dealerFinish(isTable: Boolean, seats: Seq[PlayerSeat], dealer: Hand, deck: Deck): Receive = {
    case DealerTurn ⇒
      if (seats.exists(_.hand.exists(!_.isBust)) && dealer.dealerMustHit) {
        deck.cards.toList match {
          case newCard :: newDeckCards ⇒
            context become dealerFinish(isTable, seats, dealer.addCard(newCard), Deck(newDeckCards))
            self ! DealerTurn
          case _ ⇒
            context.parent ! SendCassieMessage(BlackjackResponses.EmptyDeck)
            context.parent ! Terminated
        }
      } else {
        val results = seats.filter(_.isActive).flatMap { seat ⇒ getResult(seat, dealer) }

        context.parent ! Result(results.toMap, printHands(seats, dealer, end = true))

        if (isTable) {
          context become starting(seats.map(_.reset))
          unstashAll()
          self ! NewGameCountdown
        } else {
          context.parent ! Terminated
        }
      }

    case _ ⇒ stash()
  }

  def gameOn(isTable: Boolean, seats: Seq[PlayerSeat], dealer: Hand = Hand(), deck: Deck = Deck(),
             actionSeat: Int = 0): Receive = {

    case TableMessage(user, msg) if seats.lift(actionSeat).exists(s ⇒ s.user == user && s.hand.nonEmpty) ⇒
      msg.toLowerCase match {
        case BlackjackCommands.Hit ⇒
          val ps = seats.lift(actionSeat).get

          deck.cards.toList match {
            case newCard :: newDeckCards ⇒
              val newHand = ps.hand.map(_.addCard(newCard))

              val newSeats = seats.updated(actionSeat, ps.copy(hand = newHand))

              getNextActionSeat(newSeats, actionSeat) match {
                case Some(playerIndex) ⇒
                  context become gameOn(isTable, newSeats, dealer, Deck(newDeckCards), playerIndex)

                  context.parent ! SendCassieMessage(printHands(newSeats, dealer, end = false) +
                    printAction(newSeats.lift(playerIndex)))
                case None ⇒
                  context become dealerFinish(isTable, newSeats, dealer, Deck(newDeckCards))
                  self ! DealerTurn
              }
            case _ ⇒
              context.parent ! SendCassieMessage(BlackjackResponses.EmptyDeck)
              context.parent ! Terminated
          }

        case BlackjackCommands.Stand() ⇒
          getNextActionSeat(seats, actionSeat + 1) match {
            case Some(playerIndex) ⇒
              context become gameOn(isTable, seats, dealer, deck, playerIndex)

              context.parent ! SendCassieMessage(printHands(seats, dealer, end = false) +
                printAction(seats.lift(playerIndex)))
            case None ⇒
              context become dealerFinish(isTable, seats, dealer, deck)
              self ! DealerTurn
          }

        case _ ⇒ context.parent ! SendCassieMessage(Responses.Shrug)
      }

    case NewGameStart if seats.count(_.isActive) == 0 ⇒
      context.parent ! SendCassieMessage(BlackjackResponses.NoPlayers)
      context.parent ! Terminated

    case NewGameStart ⇒
      val (dealerCards, cardsAfterDealer) = deck.cards.splitAt(2)
      val newDealer = Hand(dealerCards)

      val (newSeats, newDeckCards) = seats.foldLeft((Seq.empty[PlayerSeat], cardsAfterDealer)) { (acc, seat) ⇒
        seat match {
          case s if s.isActive ⇒
            val (handCards, cardsLeft) = acc._2.splitAt(2)
            val newSeat = s.copy(hand = Some(Hand(handCards)))
            (acc._1 :+ newSeat, cardsLeft)
          case s ⇒ (acc._1 :+ s, acc._2)
        }
      }

      getNextActionSeat(newSeats, actionSeat) match {
        case Some(playerIndex) if !newDealer.isBlackjack⇒
          context become gameOn(isTable, newSeats, newDealer, Deck(newDeckCards), playerIndex)

          context.parent ! SendCassieMessage(printHands(newSeats, newDealer, end = false) +
            printAction(newSeats.lift(playerIndex)))
        case _ ⇒
          context become dealerFinish(isTable, newSeats, newDealer, Deck(newDeckCards))
          self ! DealerTurn
      }

    case JoinGame(user) if seats.length < 6 ⇒ // don't count splits?
      context.parent ! SendCassieMessage(BlackjackResponses.sat(user, seats.length))
      context become gameOn(isTable, seats :+ PlayerSeat(user, None), dealer, deck, actionSeat)

    case JoinGame(_) ⇒
      context.parent ! SendCassieMessage(BlackjackResponses.FullTable)
  }

  def starting(seats: Seq[PlayerSeat]): Receive = {
    case NewGameCountdown ⇒
      context.parent ! SendCassieMessage(BlackjackResponses.NewGameStarting)
      context.system.scheduler.scheduleOnce(10.seconds) {
        self ! NewGameReady
      }

    case NewGameReady ⇒
      context become gameOn(isTable = true, seats)
      self ! NewGameStart

    case TableMessage(user, GameCommands.Ante(bet)) if seats.exists(_.user == user) && canCover(user, bet) ⇒
      val newSeat = seats.find(_.user == user).get.copy(bet = Some(Try(bet.toInt).getOrElse(0)))
      val newSeats = seats.updated(seats.indexWhere(_.user == user), newSeat)
      context become starting(newSeats)

    case JoinGame(user) if seats.length < 6 ⇒
      context.parent ! SendCassieMessage(BlackjackResponses.sat(user, seats.length))
      context become starting(seats :+ PlayerSeat(user, None))

    case JoinGame(_) ⇒
      context.parent ! SendCassieMessage(BlackjackResponses.FullTable)
  }

  private def canCover(user: String, bet: String) = {
    implicit val timeout: Timeout = Timeout(5.seconds)
    val resp = context.parent ? CanPlayerCoverRequest(user, betInt(bet))
    Await.result(resp, timeout.duration).asInstanceOf[CanPlayerCoverResponse].canCover
  }

  override def receive: Receive = {
    case BJInitTable(_, user, ante) ⇒
      context become starting(Seq(PlayerSeat(user, ante)))
      self ! NewGameCountdown

    case BJInitSolo(_, user, ante) ⇒
      context become gameOn(isTable = false, Seq(PlayerSeat(user, Some(ante))))
      self ! NewGameStart

    case _ ⇒
  }
}
