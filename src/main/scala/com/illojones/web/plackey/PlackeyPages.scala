package com.illojones.web.plackey

import com.illojones.web.plackey.database.DatabaseUtil

import scala.collection.immutable.ListMap
import scalatags.Text
import scalatags.Text.all._
import scalatags.Text.tags2.title
import scalatags.stylesheet.StyleSheet

object PlackeyPages {

  def searchPage(users: ListMap[String, String], channels: ListMap[String, String],
                 results: Option[Text.TypedTag[String]] = None): Text.TypedTag[String] = {
    html(
      head(
        tag("style")(tpe := "text/css", SearchStyle.styleSheetText),
        title("plackey"),
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
          button("Search"), br, br, br,
          results
        )
      )
    )
  }

  object SearchStyle extends StyleSheet {
    initStyleSheet()

    val table = cls(
      tableLayout.fixed,
      width := 100.pct,
      borderCollapse.collapse,
      borderSpacing := 0
    )

    val data = cls(
      borderWidth := 1.px,
      borderColor := "black",
      borderStyle := "solid",
      padding := 1.px
    )

    val channel = cls(width := 130.px)
    val time = cls(width := 170.px)
    val user = cls(width := 110.px)
    val text = cls(width := 100.pct)
  }

  private final val LinkRegex = """<([^>]+)>""".r
  private final val LinkSplitRegex = """((?=<[^>]{1,1000}>)|(?<=<[^>]{1,1000}>))"""

  private def formatText(text: String) = {
    val t1 = text.replace("&amp;", "&").replace("""\\""", """\\""").replace("\\\"", "\"")

    t1.split(LinkSplitRegex).map(frag ⇒
      LinkRegex.findFirstMatchIn(frag) match {
        case None ⇒ span(frag)
        case Some(m) if m.group(1).startsWith("@") ⇒ span(frag)
        case Some(m) ⇒
          m.group(1).split('|') match {
            case Array(h, txt) ⇒ a(href := h)(txt)
            case Array(h) ⇒ a(href := h)(h)
          }
      }
    )
  }

  private def results(res: Seq[DatabaseUtil.Log], users: ListMap[String, String], channels: ListMap[String, String]) = {
    table(SearchStyle.table)(
      tr(
        th(SearchStyle.data, SearchStyle.channel)("channel"),
        th(SearchStyle.data, SearchStyle.time)("timestamp"),
        th(SearchStyle.data, SearchStyle.user)("user"),
        th(SearchStyle.data, SearchStyle.text)("text")
      ),
      res.map { log ⇒
        val channelName = channels.getOrElse(log.channel, log.channel)
        val userName = users.getOrElse(log.user, log.user)

        tr(
          td(SearchStyle.data)(channelName),
          td(SearchStyle.data)(s"${log.ts}"),
          td(SearchStyle.data)(userName),
          td(SearchStyle.data)(formatText(log.text))
        )
      }
    )
  }

  def resultsPage(res: Seq[DatabaseUtil.Log], users: ListMap[String, String],
                  channels: ListMap[String, String]): Text.TypedTag[String] = {
    searchPage(users, channels, Some(results(res, users, channels)))
  }
}
