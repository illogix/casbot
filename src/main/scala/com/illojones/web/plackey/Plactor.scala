package com.illojones.web.plackey

import akka.actor.{Actor, ActorLogging, ActorRef}
import com.illojones.web.plackey.database.DatabaseUtil

import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

class Plactor extends Actor with ActorLogging {

  val db = DatabaseUtil.getDatabase

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
