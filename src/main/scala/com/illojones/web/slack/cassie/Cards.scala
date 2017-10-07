package com.illojones.web.slack.cassie

import akka.actor.{Actor, Props}

/**
  * Created by illo on 12/14/15.
  */
case class Card(value: Int, suit: Int) {
  def getValueString: String = value match {
    case 1 => "*A*"
    case 11 => "*J*"
    case 12 => "*Q*"
    case 13 => "*K*"
    case v => s"*${v.toString}*"
  }

  def getSuitString: String = suit match {
    case 1 => ":hearts:"
    case 2 => ":clubs:"
    case 3 => ":diamonds:"
    case 4 => ":spades:"
    case _ => ":skull:"
  }

  override def toString: String = getValueString + getSuitString
}

object Deck {
  def props = Props[Deck]

  val fullDeck: Vector[Card] = {
    val seq = for {
      value <- 1 to 13
      suit <- 1 to 4
    } yield {
      Card(value, suit)
    }
    seq.toVector
  }

  case class GetCards(num: Int, freshDeck: Boolean = false)

  case class DealtCards(cards: List[Card])

}

class Deck extends Actor {

  import Deck._

  import scala.util.Random

  val rnd = new Random

  var remainingCards: Vector[Card] = fullDeck

  def getCard: Option[Card] = {
    if (remainingCards.size < 1)
      None
    else {
      val index = rnd.nextInt(remainingCards.size)
      val card = remainingCards(index)
      remainingCards = remainingCards.take(index) ++ remainingCards.takeRight(remainingCards.size - index - 1)
      Some(card)
    }

  }

  def receive = {
    case GetCards(num, freshDeck) =>
      if (freshDeck)
        remainingCards = fullDeck

      val cards = (List.empty[Option[Card]] /: (1 to num)) ((cs, _) => cs :+ getCard).flatten
      sender() ! DealtCards(cards)
  }
}
