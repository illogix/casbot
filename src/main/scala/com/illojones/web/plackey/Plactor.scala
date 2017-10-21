package com.illojones.web.plackey

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives._
import akka.pattern.{ask, pipe}
import akka.stream.{ActorMaterializer, ActorMaterializerSettings}
import akka.util.Timeout
import com.illojones.web.plackey.Plactor.{Channels, Init, Users}
import com.illojones.web.plackey.database.DatabaseUtil
import com.illojones.web.plackey.database.DatabaseUtil.{Channel, User}
import com.typesafe.config.Config

import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Plactor {
  def props(config: Config)(implicit actorSystem: ActorSystem) = Props(new Plactor(config))

  case object Init
  case class Users(users: Seq[User])
  case class Channels(channels: Seq[Channel])
}

class Plactor(config: Config)(implicit val actorSystem: ActorSystem) extends Actor with ActorLogging {
  final implicit val materializer: ActorMaterializer = ActorMaterializer(ActorMaterializerSettings(actorSystem))
  final val ErrorResponse = "¯\\_(ツ)_/¯"

  private val plackeyPath = config.getString("plackey.path")


  val db = DatabaseUtil.getDatabase(DatabaseUtil.dbConnectionUrl(config.getString("plackey.dbHost"),
    config.getString("plackey.dbDb"), config.getString("plackey.dbUser"), config.getString("plackey.dbPassword")))

  private def route(users: ListMap[String, String], channels: ListMap[String, String]) =
    path(plackeyPath) {
      get {
        complete {
          HttpEntity(ContentTypes.`text/html(UTF-8)`, PlackeyPages.searchPage(users, channels).render)
        }
      } ~
        post {
          formFields('name.?, 'after.?, 'before.?, 'channel.?, 'text.?, 'regex.?, 'limit.?, 'context.?)
            .as(PlackeyMessages.Query) { q ⇒
              implicit val askTimeout: Timeout = 30.seconds
              onSuccess((self ? q).mapTo[PlackeyMessages.Response]) { resp =>
                complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, resp.text))
              }
            } ~ {
            complete(ErrorResponse)
          }
        }
    }

  override def preStart(): Unit = {
    super.preStart()

    self ! Init
  }

  def processQuery(toReply: ActorRef, q: PlackeyMessages.Query,
                   users: ListMap[String, String], channels: ListMap[String, String]) = {
    DatabaseUtil.runQuery(db, q) onComplete {
      case Success(r) ⇒ toReply ! PlackeyMessages.Response(PlackeyPages.resultsPage(r, users, channels).render)
      case Failure(r) ⇒ toReply ! PlackeyMessages.Response(s"${r.getMessage}")
    }
  }

  private def cp(param: Option[String]) = param.flatMap(p ⇒ if (p.isEmpty) None else Some(p))

  private def cleanQuery(q: PlackeyMessages.Query) = {
    val name = q.name.flatMap(n ⇒ if (n == "!all") None else Some(n))
    val channel = q.channel.flatMap(n ⇒ if (n == "!all") None else Some(n))
    PlackeyMessages.Query(cp(name), cp(q.after), cp(q.before), cp(channel), cp(q.text), cp(q.regex),
      cp(q.limit), cp(q.context))
  }

  private def becomeReady(users: ListMap[String, String], channels: ListMap[String, String]): Unit = {
    Http().bindAndHandle(route(users, channels), "localhost", config.getInt("slack.port"))
    context become ready(users, channels)
  }

  override def receive: Receive = {
    case Init ⇒
      context become initializing(None, None)
      DatabaseUtil.getUsers(db).map(Users) pipeTo self
      DatabaseUtil.getChannels(db).map(Channels) pipeTo self
  }

  def initializing(users: Option[ListMap[String, String]], channels: Option[ListMap[String, String]]): Receive = {
    case users: Users ⇒
      val us = ListMap(users.users.map(u ⇒ u.userId → u.userName).sortBy(_._2): _*)
      channels match {
        case Some(cs) ⇒ becomeReady(us, cs)
        case None ⇒ context become initializing(Some(us), None)
      }

    case channels: Channels ⇒
      val cs = ListMap(channels.channels.map(c ⇒ c.channelId → c.channelName).sortBy(_._2): _*)
      users match {
        case Some(us) ⇒ becomeReady(us, cs)
        case None ⇒ context become initializing(None, Some(cs))
      }
  }

  def ready(users: ListMap[String, String], channels: ListMap[String, String]): Receive = {
    case q: PlackeyMessages.Query ⇒ processQuery(sender(), cleanQuery(q), users, channels)
    case _ ⇒
  }
}
