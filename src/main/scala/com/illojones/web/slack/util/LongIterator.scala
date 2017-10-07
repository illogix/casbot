package com.illojones.web.slack.util

object LongIterator {
  def from(start: Long): Iterator[Long] = from(start, 1)

  def from(start: Long, step: Long): Iterator[Long] = new Iterator[Long] {
    private var i = start
    def hasNext: Boolean = true
    def next(): Long = { val result = i; i += step; result }
  }
}