package com.illojones.web.slack

object SlackMessages {
  case class IncomingMessage(token: String, team_id: String, team_domain: String, channel_id: String,
                             channel_name: String, timestamp: String, user_id: String,
                             user_name: String, text: String)
  case class Response(text: String)
}
