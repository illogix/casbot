package com.illojones.web.plackey.database

import java.sql.Timestamp

import com.illojones.web.plackey.PlackeyMessages
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future
import scala.util.{Success, Try}

object DatabaseUtil {
  def dbConnectionUrl(dbHost: String, dbDb: String, dbUser: String, dbPassword: String) =
    s"jdbc:postgresql://$dbHost/$dbDb?user=$dbUser&password=$dbPassword"
  val dbDriver = "org.postgresql.Driver"

  val logSchema: Option[String] = None //Some("chat")
  val logTable = "chatlog"

  type DB = PostgresProfile.backend.DatabaseDef

  def getDatabase(url: String) = Database.forURL(url, driver = dbDriver)

  case class Log(id: Long, teamId: String, channelId: String, channelName: String, timestamp: Timestamp,
                 userId: String, userName: String, text: String)

  class Logs(tag: Tag) extends Table[Log](tag, logSchema, logTable) {
    def id = column[Long]("id", O.PrimaryKey)
    def teamId = column[String]("team_id")
    def channelId = column[String]("channel_id")
    def channelName = column[String]("channel_name")
    def timestamp = column[Timestamp]("timestamp")
    def userId = column[String]("user_id")
    def userName = column[String]("user_name")
    def text = column[String]("text")

    def * = (id, teamId, channelId, channelName, timestamp, userId, userName, text) <> (Log.tupled, Log.unapply)
  }

  val logs = TableQuery[Logs]

  def runQuery(db: DB, q: PlackeyMessages.Query): Future[Seq[Log]] = {

    val dbQuery = logs.filter({ log ⇒
      List(
        q.name.map(log.userName === _),
        q.after.map(a ⇒ log.timestamp > Timestamp.valueOf(s"$a 00:00:00")),
        q.before.map(b ⇒ log.timestamp < Timestamp.valueOf(s"$b 00:00:00")),
        q.channel.map(log.channelName === _),
        q.text.map(c ⇒ log.text like s"%$c%"),
      ).collect({ case Some(criteria) ⇒ criteria }).reduceLeftOption(_ && _).getOrElse(true: Rep[Boolean])
    }).sortBy(_.timestamp)

    val limit = Try(q.limit.getOrElse("1000").toInt) match {
      case Success(lmt) if lmt <= 1000 ⇒ lmt
      case _ ⇒ 1000
    }

    db.run(dbQuery.take(limit).result)
  }
}
