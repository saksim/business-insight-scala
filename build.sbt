name := "business-insight-scala"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

resolvers += Resolver.bintrayRepo("cakesolutions", "maven")

fork := true
javaOptions := Seq("-Dmx=4096M")
//exportJars := true

libraryDependencies += "com.hankcs" % "hanlp" % "portable-1.3.4"
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.10"
libraryDependencies += "com.typesafe" % "config" % "1.3.1"
libraryDependencies += "org.scalaz" % "scalaz-core_2.12" % "7.2.14"


libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.2.0",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "com.typesafe.slick" %% "slick-hikaricp" % "3.2.0"
)

