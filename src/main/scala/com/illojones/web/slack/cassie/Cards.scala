package com.illojones.web.slack.cassie

import scala.util.Random

/**
  * Created by illo on 12/14/15.
  */
case class Card(rank: Int, suit: Int) {
  def getRankString: String = rank match {
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

  override def toString: String = getRankString + getSuitString
}

object Deck {
  val fullDeck: Seq[Card] = {
    for {
      value ← 1 to 13
      suit ← 1 to 4
    } yield {
      Card(value, suit)
    }
  }

  def shuffledFullDeck: Seq[Card] = Random.shuffle(fullDeck)
}

case class Deck(cards: Seq[Card] = Deck.shuffledFullDeck)
