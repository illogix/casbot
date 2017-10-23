package com.illojones.web.slack.cassie.games

import akka.actor.{Actor, ActorLogging}
import com.illojones.web.slack.cassie.games.GameActor._

object GameActor {
  trait InitMessage
  trait SoloInitMessage extends InitMessage
  trait TableInitMessage extends InitMessage

  trait GameState
  trait PlayerState
  case class Response(message: String, channel: Option[String] = None)

  case object Terminated
}

abstract class GameActor extends Actor with ActorLogging {
//  var state: GameState
//  var players: Map[String, PlayerState]
}
