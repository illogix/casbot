package com.illojones.web.slack.casbot.games

import akka.actor.Actor
import com.illojones.web.slack.casbot.Card

class Blackjack extends Actor {
  var playerCards: List[Card] = List.empty
  var dealerCards: List[Card] = List.empty

  override def preStart(): Unit = {
    super.preStart()

  }

  override def receive: Receive = {
    case _ ⇒
  }
}
