package com.illojones.web.plackey.database

import java.sql.Timestamp

import com.illojones.web.plackey.PlackeyMessages
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}

object DatabaseUtil {
  def dbConnectionUrl(dbHost: String, dbDb: String, dbUser: String, dbPassword: String) =
    s"jdbc:postgresql://$dbHost/$dbDb?user=$dbUser&password=$dbPassword"
  val dbDriver = "org.postgresql.Driver"

  val logSchema: Option[String] = None
  val logTable = "chatlog"

  type DB = PostgresProfile.backend.DatabaseDef

  def getDatabase(url: String) = Database.forURL(url, driver = dbDriver)

  case class Log(id: Option[Long], channel: String, user: String, ts: Timestamp, text: String)

  class Logs(tag: Tag) extends Table[Log](tag, logSchema, logTable) {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def channel = column[String]("channel")
    def user = column[String]("user")
    def ts = column[Timestamp]("timestamp")
    def text = column[String]("text")

    def * = (id.?, channel, user, ts, text) <> (Log.tupled, Log.unapply)
  }

  val logs = TableQuery[Logs]


  def addLog(db: DB, log: Log): Future[Int] = {
    db.run(logs insertOrUpdate log)
  }

  def runQuery(db: DB, q: PlackeyMessages.Query): Future[Seq[Log]] = {

    val dbQuery = logs.filter({ log ⇒
      List(
        q.name.map(log.user === _),
        q.after.map(a ⇒ log.ts > Timestamp.valueOf(s"$a 00:00:00")),
        q.before.map(b ⇒ log.ts < Timestamp.valueOf(s"$b 00:00:00")),
        q.channel.map(log.channel === _),
        q.text.map(c ⇒ log.text like s"%$c%"),
      ).collect({ case Some(criteria) ⇒ criteria }).reduceLeftOption(_ && _).getOrElse(true: Rep[Boolean])
    }).sortBy(_.ts)

    val limit = Try(q.limit.getOrElse("1000").toInt) match {
      case Success(lmt) if lmt <= 1000 ⇒ lmt
      case _ ⇒ 1000
    }

    db.run(dbQuery.take(limit).result)
  }

  def createTableIfNeeded(db: DB): Future[List[Unit]] = {
    for {
      mts ← db.run(MTable.getTables)
      names = mts.map(_.name.name)
      tables = List(logs)
      creates = tables.filter(t ⇒ !names.contains(t.baseTableRow.tableName)).map(_.schema.create)
      res ← db.run(DBIO.sequence(creates))
    } yield {
      res
    }
  }

}
