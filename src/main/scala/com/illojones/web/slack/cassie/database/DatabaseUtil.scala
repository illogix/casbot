package com.illojones.web.slack.cassie.database

import com.illojones.web.slack.cassie.Cassie.Player
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DatabaseUtil {
  def dbConnectionUrl(dbHost: String, dbDb: String, dbUser: String, dbPassword: String) =
    s"jdbc:postgresql://$dbHost/$dbDb?user=$dbUser&password=$dbPassword"
  val dbDriver = "org.postgresql.Driver"

  type DB = PostgresProfile.backend.DatabaseDef

  def getDatabase(url: String) = Database.forURL(url, driver = dbDriver)

  case class DBPlayer(user: String, balance: Int)

  class Players(tag: Tag) extends Table[DBPlayer](tag, "players") {
    def user = column[String]("user", O.PrimaryKey)
    def balance = column[Int]("balance")
    def * = (user, balance) <> (DBPlayer.tupled, DBPlayer.unapply)
  }

  val players = TableQuery[Players]

  def createTableIfNeeded(db: DB): Future[List[Unit]] = {
    for {
      mts ← db.run(MTable.getTables)
      names = mts.map(_.name.name)
      tables = List(players)
      creates = tables.filter(t ⇒ !names.contains(t.baseTableRow.tableName)).map(_.schema.create)
      res ← db.run(DBIO.sequence(creates))
    } yield {
      res
    }
  }

  def updatePlayer(db: DB, p: Player): Future[Int] = {
    val upsert = players insertOrUpdate DBPlayer(p.user, p.balance)

    db.run(upsert)
  }

  def getPlayers(db: DB): Future[Seq[Player]] = {
    val query = players.result

    db.run(query).map(_.map(dbp ⇒ Player(dbp.user, dbp.balance)))
  }
}
