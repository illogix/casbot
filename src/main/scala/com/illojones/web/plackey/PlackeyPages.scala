package com.illojones.web.plackey

import com.illojones.web.plackey.database.DatabaseUtil

import scala.collection.immutable.ListMap
import scalatags.Text.all._
import scalatags.Text.tags2.title
object PlackeyPages {


  def searchPage(users: Map[String, String], channels: Map[String, String]) = {
    html(
      head(
        title("plackey")
      ),
      body(
        h2("Log Searcher"),
        form(method := "post")(
          "From: ", select(name := "name")(
            option(value := "!all")("-All Users-"),
            users.map({case (id: String, name: String) ⇒ option(value := id)(name)}).toSeq
          ), br,
          "After: ", input(name := "after", `type` := "date"), br,
          "Before: ", input(name := "before", `type` := "date"), br,
          "Channel: ", select(name := "channel")(
            option(value := "!all")("-All Channels-"),
            channels.map({case (id: String, name: String) ⇒ option(value := id)(name)}).toSeq
          ), br,
          "Containing: ", input(name := "text", `type` := "text"),
          input(name := "regex", `type` := "checkbox")("Regex"), br,
          "Limit: ", input(name := "limit", `type` := "text", value := "1000"), br,
          br, br,
          button("Search")
        )
      )
    )
  }

  lazy final val resultsPageHeader =
    """
      | <html>
      | <head>
      |   <style type="text/css">
      |   table {
      |    table-layout: fixed;
      |    width: 100%;
      |    border-collapse: collapse;
      |    border-spacing: 0;
      |}
      |
      |th,td {
      |    border: 1px solid black;
      |    padding: 1px;
      |}
      |
      |th.channel {
      |    width: 130px;
      |}
      |
      |th.time {
      |    width: 170px;
      |}
      |
      |th.user {
      |    width: 110px;
      |}
      |
      |th.text {
      |    width: 100%;
      |}
      |</style>
      | </head>
      | <body>
    """.stripMargin

  def resultsTable(res: Seq[DatabaseUtil.Log], users: ListMap[String, String],
                   channels: ListMap[String, String]): String = {
    val header = "<table><tr><th class=\"channel\">channel</th><th class=\"time\">timestamp</th><th class=\"user\">user</th><th class=\"text\">text</th></tr>"
    val results = res.map { log ⇒

      val channelName = channels.getOrElse(log.channel, log.channel)
      val userName = users.getOrElse(log.user, log.user)

      s"<tr><td>$channelName</td><td>${log.ts}</td><td>$userName</td>" +
        s"<td>${log.text.replace("&amp;", "&")}</td></tr>"
    }

    s"$resultsPageHeader$header${results.mkString("\n")}</table></body></html>"
  }
}
