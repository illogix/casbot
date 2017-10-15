package com.illojones.web

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import com.illojones.web.plackey.Plactor
import com.illojones.web.slack.Bot
import com.typesafe.config.ConfigFactory


object Main extends App {
  implicit val actorSystem: ActorSystem = ActorSystem("system")
  final implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))

  val cf = ConfigFactory.load()
  val bot = actorSystem.actorOf(Bot.props(cf), "bot")

  val plactor = actorSystem.actorOf(Plactor.props(cf))

}
