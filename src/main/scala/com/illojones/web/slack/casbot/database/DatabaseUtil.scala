package com.illojones.web.slack.casbot.database

import com.illojones.web.slack.casbot.Casbot.Player
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DatabaseUtil {
  val dbHost = "localhost"
  val dbDb = "illo"
  val dbUser = "illo"
  val dbPassword = ""
  val dbConnectionUrl = s"jdbc:postgresql://$dbHost/$dbDb?user=$dbUser&password=$dbPassword"
  val dbDriver = "org.postgresql.Driver"

  type DB = PostgresProfile.backend.DatabaseDef

  def getDatabase = Database.forURL(dbConnectionUrl, driver = dbDriver)

  case class DBPlayer(id: String, name: String, balance: Int)

  class Players(tag: Tag) extends Table[DBPlayer](tag, "players") {
    def id = column[String]("id", O.PrimaryKey)
    def name = column[String]("name")
    def balance = column[Int]("balance")
    def * = (id, name, balance) <> (DBPlayer.tupled, DBPlayer.unapply)
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
    val upsert = players insertOrUpdate DBPlayer(p.id, p.name, p.balance)

    db.run(upsert)
  }

  def getPlayers(db: DB): Future[Seq[Player]] = {
    val query = players.result

    db.run(query).map(_.map(dbp ⇒ Player(dbp.id, dbp.name, dbp.balance)))
  }
}
