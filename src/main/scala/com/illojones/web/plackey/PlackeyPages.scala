package com.illojones.web.plackey

import java.net.URLDecoder

import com.illojones.web.plackey.database.DatabaseUtil

object PlackeyPages {

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

  def resultsTable(res: Seq[DatabaseUtil.Log]): String = {
    val header = "<table><tr><th class=\"channel\">channel</th><th class=\"time\">timestamp</th><th class=\"user\">user</th><th class=\"text\">text</th></tr>"
    val results = res.map(log ⇒
      s"<tr><td>${log.channelName}</td><td>${log.timestamp}</td><td>${log.userName}</td>" +
        s"<td>${xml.Utility.escape(URLDecoder.decode(log.text, "UTF-8")).replace("&amp;", "&")}</td></tr>"
    )

    s"$resultsPageHeader$header${results.mkString("\n")}</table></body></html>"
  }
}
