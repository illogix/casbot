//package org.samyi.casbot.bots.casbot.games
//
//import akka.actor.{ActorLogging, Actor, ActorRef, Props}
//import com.newrelic.agent.commands.Command
//import org.samyi.casbot.Req
//import org.samyi.casbot.BotResponse
//import org.samyi.casbot.bots.casbot._
//
///**
// * Created by illo on 3/13/16.
// */
//object Blackjack2 {
//  def props(player: ActorRef, user: String) = Props(classOf[Blackjack], player, user)
//
//  val minimum: Long = 10L
//  val HIT = "hit"
//  val STAND = "stand"
//  val DOUBLE = "double"
//  val SPLIT = "split"
//
//  case class Ante(amt: Long)
//
//  sealed trait State
//
//  sealed trait Result extends State
//
//  case object Win extends Result { override def toString = "Player wins!" }
//
//  case object Lose extends Result { override def toString = "Player loses." }
//
//  case object Push extends Result { override def toString = "Push." }
//
//  case object Ready extends State { override def toString = "Waiting for player..." }
//
//  case object DealerMustHit extends State { override def toString = "Dealer must hit..." }
//
//  def getCardValue(card: Card): Int = card.value match {
//    case 1 => 11
//    case v if v < 11 => v
//    case _ => 10
//  }
//
//  case class Hand(cards: List[Card] = Nil, value: Int = 0, soft: Boolean = false) {
//    def add(card: Card) = {
//      val softCard = card.value == 1
//
//      val (newVal, newSoft) = value + getCardValue(card) match {
//        case v if v > 21 && (soft || softCard) => (v - 10, soft && softCard)
//        case v => (v, soft || softCard)
//      }
//
//      Hand(cards :+ card, newVal, newSoft)
//    }
//
//    def canDouble = cards.length == 2
//    def canSplit = cards.length == 2 && cards.head.value == cards(1).value
//  }
//
//  def post(msg: String) = Casbot.post(msg)
//
//}
//
//class Blackjack2(player: ActorRef, user: String) extends CasGame {
//
//  import Blackjack2._
//
//  var dealerHand = Hand()
//  var playerHand = Hand()
//  var bet: Long = 0L
//
//  val deck = context.system.actorOf(Deck.props)
//
//  var dealerTurn: Boolean = false
//
//  def ante(user_name: String, betStr: String): BotResponse = {
//    try {
//      val bet = betStr.toLong
//      if (bet < Blackjack2.minimum)
//        BotResponse(Some(s"Sorry, table minimum is ${Blackjack2.minimum}"))
//      else {
//        self ! Blackjack2.Ante(bet)
//        BotResponse(None)
//      }
//    } catch {
//      case e: Exception => BotResponse(Some(s"invalid bet: $betStr"))
//    }
//  }
//
//  private def getState: State = {
//    def isBlackjack(hand: Hand) = hand.cards.length == 2 && hand.value == 21
//
//    val pbj = isBlackjack(playerHand)
//    val dbj = isBlackjack(dealerHand)
//
//    if (pbj && dbj) Push
//    else if (pbj) Win
//    else if (dbj) Lose
//    else if (dealerHand.value > 21) Win
//    else if (playerHand.value > 21) Lose
//    else if (!dealerTurn) Ready
//    else if (dealerHand.value < 17 || (dealerHand.value == 17 && dealerHand.soft)) DealerMustHit
//    else if (dealerHand.value > playerHand.value) Lose
//    else if (playerHand.value > dealerHand.value) Win
//    else Push
//  }
//
//  private def postState(state: State, pbo: Option[Player.PlayerBalance] = None): Unit = {
//    val showDealer = state match {
//      case r: Result => true
//      case _ => dealerTurn
//    }
//
//    val dealerString = if (showDealer) {
//      ("Dealer: " /: dealerHand.cards)((s, c) => s"$s $c") + s" [${dealerHand.value}]"
//    } else {
//      s"Dealer: ${dealerHand.cards.head} :black_joker:"
//    }
//    val playerString = (s" $user: " /: playerHand.cards)((s, c) => s"$s $c") + s" [${playerHand.value}]"
//
//    val stateString = s"$dealerString $playerString $state"
//
//    pbo match {
//      case Some(pb) => Casbot.post(s"$stateString (${pb.name}: $$${pb.balance})")
//      case None => Casbot.post(stateString)
//    }
//  }
//
//  private def reset(): Unit = {
//    bet = 0L
//    dealerHand = Hand()
//    playerHand = Hand()
//    dealerTurn = false
//    context become receive
//  }
//
//  def receive = {
//    case Casbot.Command(u, text) => text match {
//      case t if t.startsWith("ante+") => sender() ! ante(user, t.substring(5))
//      case _ => sender() ! BotResponse(Some("Blackjack: unknown command"))
//    }
//
//    case Ante(amt) =>
//      bet = amt
//      post(s"$user has bet $amt, starting game")
//      context become dealing
//      deck ! Deck.GetCards(4, freshDeck = true)
//
//    case _ => error("unexpected message in receive state")
//  }
//
//  def dealing: Receive = {
//    case Deck.DealtCards(cards) => cards match {
//      case p1 :: d1 :: p2 :: d2 :: Nil =>
//        dealerHand = dealerHand.add(d1).add(d2)
//        playerHand = playerHand.add(p1).add(p2)
//        getState match {
//          case Ready =>
//            postState(Ready)
//            context become waitingPlayer
//          case r: Result => finish(r)
//          case _ => error("unexpected message in dealing state")
//        }
//      case _ => error("dealing fail")
//    }
//    case _ => error("unexpected message in dealing state")
//  }
//
//  def waitingPlayer: Receive = {
//    case Casbot.Command(u, text) => text match {
//      case HIT =>
//        deck ! Deck.GetCards(1)
//        sender() ! BotResponse(None)
//      case STAND =>
//        dealerTurn = true
//        context.become(dealersTurn(sender()))
//        self ! getState
//        sender() ! BotResponse(None)
//      case DOUBLE =>
//        sender() ! BotResponse(Some("double not implemented yet"))
//      case SPLIT =>
//        sender() ! BotResponse(Some("split not implemented yet"))
//      case t => sender() ! BotResponse(Some(s"unknown command: $t"))
//    }
//
//    case Deck.DealtCards(card :: Nil) =>
//      playerHand = playerHand.add(card)
//      getState match {
//        case r: Result => finish(r)
//        case state => postState(state)
//      }
//
//    case _ => error("unexpected message in waitingPlayer state")
//  }
//
//  def dealersTurn(sender: ActorRef): Receive = {
//    case DealerMustHit => deck ! Deck.GetCards(1)
//
//    case r: Result => finish(r)
//
//    case Deck.DealtCards(card :: Nil) =>
//      dealerHand = dealerHand.add(card)
//      self ! getState
//
//    case _ => error("unexpected message in dealersTurn state")
//  }
//
//  def done(result: Result): Receive = {
//    case pb: Player.PlayerBalance =>
//      postState(result, Some(pb))
//      reset()
//  }
//
//  def finish(result: Result) = {
//    context.become(done(result))
//    result match {
//      case Win => player ! Player.UpdateBalance(bet)
//      case Lose => player ! Player.UpdateBalance(0 - bet)
//      case _ => player ! Player.GetBalance
//    }
//  }
//
//  def error(msg: String) = {
//    post(msg)
//    reset()
//  }
//}