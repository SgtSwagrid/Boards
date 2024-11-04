import play.sbt.PlayImport.guice
import sbt.Keys.libraryDependencies
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / version := "4.0-beta"
ThisBuild / organization := "swagco"

ThisBuild / scalaVersion := "3.5.2"
ThisBuild / scalacOptions ++= Seq (
  /*"-experimental",
  "-feature",*/
  //"-language:implicitConversions",
  //"-language:namedTuples",
  "-Xmax-inlines", "128",
)

Global / onChangedBuildSource := ReloadOnSourceChanges

val V = new {
  val circe = "0.14.9"
  val laminext = "0.17.0"
}

val Lib = new {
  val circe = libraryDependencies ++= Seq (
    "io.circe" %%% "circe-core" % V.circe,
    "io.circe" %%% "circe-generic" % V.circe,
    "io.circe" %%% "circe-parser" % V.circe,
  )
  val laminext = libraryDependencies ++= Seq (
    "io.laminext" %%% "core" % V.laminext,
    "io.laminext" %%% "fetch" % V.laminext,
    "io.laminext" %%% "fetch-circe" % V.laminext,
    "io.laminext" %%% "websocket" % V.laminext,
    "io.laminext" %%% "websocket-circe" % V.laminext,
  )
}

lazy val root = project
  .in(file("."))
  .aggregate(server, client, common.jvm, common.js, games.jvm, games.js)

lazy val server = project
  .in(file("server"))
  .dependsOn(common.jvm, games.jvm)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings (
    name := "boards-server",
    javaOptions ++= Seq (
      "-Dplay.editor=http://localhost:63342/api/file/?file=%s&line=%s",
    ),
    libraryDependencies ++= Seq (
      "org.typelevel" %% "cats-core" % "2.12.0",
      "com.typesafe.slick" %% "slick" % "3.5.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1",
      "org.slf4j" % "slf4j-api" % "2.0.13",
      "org.playframework" %% "play-slick" % "6.1.0",
      "org.playframework" %% "play-slick-evolutions" % "6.1.1",
      "com.h2database" % "h2" % "2.2.224",
      "org.mindrot" % "jbcrypt" % "0.4",
      "com.vmunier" %% "scalajs-scripts" % "1.3.0",
      guice
    ),
    Lib.circe,
    dependencyOverrides += "org.slf4j" % "slf4j-api" % "2.0.13",
    scalaJSProjects := Seq(client),
    Assets / pipelineStages := Seq(scalaJSPipeline),
    Assets / WebKeys.packagePrefix := "public/",
    Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline).value,
  )

lazy val client = project
  .in(file("client"))
  .dependsOn(common.js, games.js)
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .settings (
    name := "boards-client",
    libraryDependencies ++= Seq (
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.0.0",
      "com.raquo" %%% "airstream" % "17.0.0",
    ),
    Lib.circe,
    Lib.laminext,
  )

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(games)
  .enablePlugins(ScalaJSWeb)
  .in(file("common"))
  .settings (
    name := "boards-common",
    Lib.circe,
  )

lazy val games = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(ScalaJSWeb)
  .in(file("games"))
  .settings (
    name := "boards-games",
  )