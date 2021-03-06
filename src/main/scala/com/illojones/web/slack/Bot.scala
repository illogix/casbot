package com.illojones.web.slack

import akka.Done
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.{Flow, Keep, Sink, Source, SourceQueueWithComplete}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import com.illojones.web.slack.Bot._
import com.illojones.web.slack.SlackMessages._
import com.illojones.web.slack.util.LongIterator
import com.typesafe.config.Config
import spray.json._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Bot {
  def props(config: Config)(implicit system: ActorSystem, materializer: ActorMaterializer) = Props(new Bot(config))

  case object Init
  case class WebSocketConnectSuccess(queue: SourceQueueWithComplete[Message], closed: Future[Done])
  case object WebSocketDisconnected
  case class SendMessage(channel: String, message: String)

  case class ApiError(code: String) extends Exception(code)
  case class InvalidResponseError(status: Int, body: String) extends Exception(s"Invalid response: $status")

  case class ApiRequest(url: String)
  case class ApiResponse(fields: Map[String, JsValue])
}

class Bot(config: Config)(implicit val system: ActorSystem, val materializer: ActorMaterializer)
  extends Actor with ActorLogging with SlackMessageSerialization {

  import akka.pattern.pipe
  import context.dispatcher

  private val msgHandler = context.actorOf(MessageHandler.props(config), "msgHandler")

  private val token = config.getString("slack.apiToken")
  private val connectUrl = config.getString("slack.connectUrl")

  val http = Http(context.system)

  private def makeApiRequest(request: HttpRequest)(implicit system: ActorSystem): Future[ApiResponse] = {
    Http().singleRequest(request).flatMap {
      case response if response.status.intValue == 200 ⇒
        response.entity.toStrict(10.seconds).map { entity ⇒
          val jsFields = JsonParser(ParserInput(entity.data.decodeString("UTF-8"))).asJsObject.fields
          ApiResponse(jsFields)
        }

      case response ⇒
        response.entity.toStrict(10.seconds).map { entity ⇒
          ApiResponse(Map.empty)
        }
    }
  }


  override def preStart(): Unit = {
    super.preStart()

    self ! Init
  }

  private var outboundMessageQueue: Option[SourceQueueWithComplete[Message]] = None

  private val idIter = LongIterator.from(1)

  def httpReq(url: String) = HttpRequest(uri = Uri(url).withQuery(Query(Map("token" → token))))

  override def receive: Receive = {

    case Init ⇒ http.singleRequest(httpReq(connectUrl)).pipeTo(self)

    case ar: ApiRequest ⇒ makeApiRequest(httpReq(ar.url)).pipeTo(sender())

    case m: Message ⇒ msgHandler ! m

    case SendMessage(channel, message) ⇒
      val resp = OutgoingMessage(idIter.next().toString, channel, message)
      if (outboundMessageQueue.isDefined) {
        outboundMessageQueue.get.offer(TextMessage(resp.toJson.compactPrint))
      }
    case WebSocketConnectSuccess(queue, closed) ⇒
      outboundMessageQueue = Some(queue)
      closed.onComplete(_ ⇒ self ! WebSocketDisconnected)

    case WebSocketDisconnected ⇒
      log.info("WebSocket disconnected, re-initializing")
      self ! Init

    case HttpResponse(StatusCodes.OK, headers, entity, _) ⇒
      Unmarshal(entity).to[ConnectResponse] onComplete {
        case Success(cr) ⇒

          val messageSink: Sink[Message, Future[Done]] =
            Sink.foreach { message ⇒ self ! message }

          val queueSource: Source[Message, SourceQueueWithComplete[Message]] =
            Source.queue[Message](1000, OverflowStrategy.dropHead)

          val flow: Flow[Message, Message, (Future[Done], SourceQueueWithComplete[Message])] =
            Flow.fromSinkAndSourceMat(messageSink, queueSource)(Keep.both)

          val (upgradeResponse, (closed, messageSourceQueue)) = Http().singleWebSocketRequest(WebSocketRequest(cr.url), flow)

          upgradeResponse.onComplete {
            case Success(upgrade) if upgrade.response.status == StatusCodes.SwitchingProtocols ⇒
              log.info("Web socket connection success")
              self ! WebSocketConnectSuccess(messageSourceQueue, closed)

            case Success(upgrade) ⇒
              log.info("Web socket connection failed: {}", upgrade.response)
              context.stop(self)

            case Failure(err) ⇒
              log.info("Web socket connection failed with error: {}", err.getMessage)
              context.stop(self)
          }

        case Failure(t) ⇒
          log.error(s"Re-initializing after error unmarshalling ConnectResponse: ${t.getMessage}")
          self ! Init
      }

    case resp @ HttpResponse(code, _, _, _) ⇒
      log.info("Request failed, response code: " + code)
      resp.discardEntityBytes()
  }

}
