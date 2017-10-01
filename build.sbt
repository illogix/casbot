name := "casbot"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.0.10",
//  "com.typesafe.akka" %% "akka-http-testkit" % "10.0.10" % Test
  "com.typesafe.slick" %% "slick" % "3.2.1",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.1",
  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
)
