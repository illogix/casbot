import java.io.{File, PrintWriter}
import java.net.URLDecoder

object MigrateOldDatabase extends App {
  val bufferedSource = io.Source.fromFile("/tmp/dump.sql")

  val record = """([^(]+)\((\d+), '[^']*', '([^']*)', '[^']*', '([^']*)', '([^']*)', '[^']*', '([^']*)'\);""".r

  val writer = new PrintWriter(new File("/tmp/newdump.sql"))

  for (line <- bufferedSource.getLines) {
    line match {
      case record(insert, id, channel, ts, user, encodedText) ⇒
        val text = URLDecoder.decode(encodedText, "UTF-8").replaceAll("'", "''")

        writer.write(s"$insert($id, '$channel', '$user', '$ts', '$text');\n")

      case badline ⇒ println(s"Line doesn't match: $badline")
    }
  }

  writer.close()
  bufferedSource.close

  println("Done!")
}
