package com.illojones.web.slack.cassie

import scala.util.Try

object CassieUtils {
  def betInt(bet: Option[String]): Int = bet.flatMap(b ⇒ Try(b.toInt).toOption).getOrElse(0)
  def betInt(bet: String): Int = Try(bet.toInt).toOption.getOrElse(0)
}
