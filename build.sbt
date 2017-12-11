name := "Iteration4"

version := "1.0"

scalaVersion := "2.12.1"


libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.5.2",
  "org.asynchttpclient" % "async-http-client" % "2.0.32",
  "org.slf4j" % "slf4j-simple" % "1.7.21" % "runtime",
  "org.specs2" %% "specs2-core" % "3.8.6" % Test
)
