package com.illojones.web.plackey

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.illojones.web.plackey.database.DatabaseUtil
import com.typesafe.config.Config

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Plactor {
  def props(config: Config) = Props(new Plactor(config))
}

class Plactor(config: Config) extends Actor with ActorLogging {

  val db = DatabaseUtil.getDatabase(DatabaseUtil.dbConnectionUrl(config.getString("plackey.dbHost"),
    config.getString("plackey.dbDb"), config.getString("plackey.dbUser"), config.getString("plackey.dbPassword")))

  def processQuery(q: PlackeyMessages.Query, toReply: ActorRef) = {
    DatabaseUtil.runQuery(db, q) onComplete {
      case Success(r) ⇒ toReply ! PlackeyMessages.Response(PlackeyPages.resultsTable(r))
      case Failure(r) ⇒ toReply ! PlackeyMessages.Response(s"${r.getMessage}")
    }
  }

  private def cp(param: Option[String]) = param.flatMap(p ⇒ if (p.isEmpty) None else Some(p))

  private def cleanQuery(q: PlackeyMessages.Query) = {
    PlackeyMessages.Query(cp(q.name), cp(q.after), cp(q.before), cp(q.channel), cp(q.text), cp(q.regex),
      cp(q.limit), cp(q.context))
  }

  override def receive: Receive = {
    case q: PlackeyMessages.Query ⇒
      log.info(s"${cleanQuery(q)}")
      processQuery(cleanQuery(q), sender())

    case _ ⇒
  }
}
