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

  val schema: Option[String] = None
  val logTable = "chatlog"
  val channelTable = "chatchannels"
  val userTable = "chatusers"

  type DB = PostgresProfile.backend.DatabaseDef

  def getDatabase(url: String) = Database.forURL(url, driver = dbDriver)

  case class Log(id: Option[Long], channel: String, user: String, ts: Timestamp, text: String)

  class Logs(tag: Tag) extends Table[Log](tag, schema, logTable) {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def channel = column[String]("channel")
    def user = column[String]("user")
    def ts = column[Timestamp]("timestamp")
    def text = column[String]("text")

    def * = (id.?, channel, user, ts, text) <> (Log.tupled, Log.unapply)
  }

  val logs = TableQuery[Logs]


  def addLog(db: DB, log: Log): Future[Int] = db.run(logs insertOrUpdate log)

  def getUsers(db: DB): Future[Seq[User]] = db.run(users.result)

  def getChannels(db: DB): Future[Seq[Channel]] = db.run(channels.result)

  def updateUsers(db: DB, us: Vector[User]): Future[Option[Int]] = {
    getUsers(db) flatMap { items ⇒
      val newItems = us filterNot { u ⇒ items.exists(_.userId == u.userId) }
      db.run(users ++= newItems)
    }
  }

  def updateChannels(db: DB, cs: Vector[Channel]): Future[Option[Int]] = {
    getChannels(db) flatMap { items ⇒
      val newItems = cs filterNot { c ⇒ items.exists(_.channelId == c.channelId) }
      db.run(channels ++= newItems)
    }
  }

  implicit class RegexLikeOps(s: Rep[String]) {
    def regexLike(p: Rep[String]): Rep[Boolean] = {
      val expr = SimpleExpression.binary[String,String,Boolean] { (s, p, qb) =>
        qb.expr(s)
        qb.sqlBuilder += " ~* "
        qb.expr(p)
      }
      expr.apply(s,p)
    }
  }

  def runQuery(db: DB, q: PlackeyMessages.Query): Future[Seq[Log]] = {

    val dbQuery = logs.filter({ log ⇒
      List(
        q.name.map(log.user === _),
        q.after.map(a ⇒ log.ts > Timestamp.valueOf(s"$a 00:00:00")),
        q.before.map(b ⇒ log.ts < Timestamp.valueOf(s"$b 00:00:00")),
        q.channel.map(log.channel === _),
        q.text.map(c ⇒ if (q.regex.nonEmpty) log.text regexLike c else log.text like s"%$c%"),
      ).collect({ case Some(criteria) ⇒ criteria }).reduceLeftOption(_ && _).getOrElse(true: Rep[Boolean])
    }).sortBy(_.ts)

    val limit = Try(q.limit.getOrElse("1000").toInt) match {
      case Success(lmt) if lmt <= 1000 ⇒ lmt
      case _ ⇒ 1000
    }

    db.run(dbQuery.take(limit).result)
  }

  def createTablesIfNeeded(db: DB): Future[List[Unit]] = {
    for {
      mts ← db.run(MTable.getTables)
      names = mts.map(_.name.name)
      tables = List(logs, channels, users)
      creates = tables.filter(t ⇒ !names.contains(t.baseTableRow.tableName)).map(_.schema.create)
      res ← db.run(DBIO.sequence(creates))
    } yield {
      res
    }
  }

  case class Channel(id: Option[Long], channelId: String, channelName: String)

  class Channels(tag: Tag) extends Table[Channel](tag, schema, channelTable) {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def channelId = column[String]("channel_id")
    def channelName = column[String]("channel_name")

    def * = (id.?, channelId, channelName) <> (Channel.tupled, Channel.unapply)
  }

  val channels = TableQuery[Channels]

  case class User(id: Option[Long], userId: String, userName: String)

  class Users(tag: Tag) extends Table[User](tag, schema, userTable) {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def userId = column[String]("user_id")
    def userName = column[String]("user_name")

    def * = (id.?, userId, userName) <> (User.tupled, User.unapply)
  }

  val users = TableQuery[Users]

}
