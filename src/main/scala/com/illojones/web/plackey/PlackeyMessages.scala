package com.illojones.web.plackey

object PlackeyMessages {
  case class Query(name: Option[String],
                   after: Option[String],
                   before: Option[String],
                   channel: Option[String],
                   text: Option[String],
                   regex: Option[String],
                   limit: Option[String],
                   context: Option[String])
  case class Response(text: String)
}
