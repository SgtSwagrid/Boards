import sbt.Keys.libraryDependencies
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}
import play.sbt.PlayImport.guice

ThisBuild / version := "4.0-beta"
ThisBuild / organization := "swagco"

ThisBuild / scalaVersion := "3.6.2"
ThisBuild / scalacOptions ++= Seq (
  "-Xmax-inlines", "128",
)

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / scalaJSStage := FullOptStage

val V = new {
  val slick     = "3.5.2"
  val playSlick = "6.1.1"
  val laminar   = "17.0.0"
  val laminext  = "0.17.0"
  val circe     = "0.14.9"
}

val Lib = new {
  val slick = libraryDependencies ++= Seq (
    "com.typesafe.slick" %% "slick"                 % V.slick,
    "com.typesafe.slick" %% "slick-hikaricp"        % V.slick,
    "org.playframework"  %% "play-slick"            % V.playSlick,
    "org.playframework"  %% "play-slick-evolutions" % V.playSlick,
  )
  val laminar = libraryDependencies ++= Seq (
    "com.raquo"   %%% "laminar"         % V.laminar,
    "com.raquo"   %%% "airstream"       % V.laminar,
    "io.laminext" %%% "core"            % V.laminext,
    "io.laminext" %%% "fetch"           % V.laminext,
    "io.laminext" %%% "fetch-circe"     % V.laminext,
    "io.laminext" %%% "websocket"       % V.laminext,
    "io.laminext" %%% "websocket-circe" % V.laminext,
  )
  val circe = libraryDependencies ++= Seq (
    "io.circe" %%% "circe-core"    % V.circe,
    "io.circe" %%% "circe-generic" % V.circe,
    "io.circe" %%% "circe-parser"  % V.circe,
  )
}

lazy val root = project
  .in(file("."))
  .aggregate (
    server,
    client,
    common.jvm, common.js,
    dsl.jvm,    dsl.js,
    games.jvm,  games.js,
    bots.jvm,   bots.js,
  )

lazy val server = project
  .in(file("server"))
  .dependsOn(common.jvm, dsl.jvm, games.jvm, bots.jvm)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings (
    name := "boards-server",
    javaOptions ++= Seq (
      "-Dplay.editor=http://localhost:63342/api/file/?file=%s&line=%s",
      "-Xmx8G",
    ),
    libraryDependencies ++= Seq (
      "com.h2database" %  "h2"              % "2.3.232",
      "org.mindrot"    %  "jbcrypt"         % "0.4",
      "org.slf4j"      %  "slf4j-api"       % "2.0.16",
      "com.vmunier"    %% "scalajs-scripts" % "1.3.0",
      guice,
    ),
    Lib.slick,
    Lib.circe,
    dependencyOverrides += "org.slf4j" % "slf4j-api" % "2.0.16",
    scalaJSProjects := Seq(client, common.js, dsl.js, games.js, bots.js),
    Assets / pipelineStages := Seq(scalaJSPipeline),
    Assets / WebKeys.packagePrefix := "public/",
    Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline).value,
  )

lazy val client = project
  .in(file("client"))
  .dependsOn(common.js, dsl.js, games.js, bots.js)
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .settings (
    name := "boards-client",
    libraryDependencies ++= Seq (
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
    ),
    Lib.laminar,
    Lib.circe,
  )

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(dsl, games, bots)
  .enablePlugins(ScalaJSWeb)
  .in(file("common"))
  .settings (
    name := "boards-common",
    Lib.circe,
  )

lazy val dsl = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .enablePlugins(ScalaJSWeb)
  .in(file("dsl"))
  .settings (
    name := "boards-dsl",
    Lib.circe,
  )

lazy val games = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(dsl)
  .enablePlugins(ScalaJSWeb)
  .in(file("games"))
  .settings (
    name := "boards-games",
    Lib.circe,
  )

lazy val bots = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .dependsOn(dsl)
  .enablePlugins(ScalaJSWeb)
  .in(file("bots"))
  .settings (
    name := "boards-bots",
    Lib.circe,
  )