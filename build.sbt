import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import sbt.Keys.libraryDependencies

ThisBuild / version := "4.0-beta"
ThisBuild / scalaVersion := "3.4.2"
ThisBuild / organization := "swagco"

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = project
  .in(file("."))
  .aggregate(server, client, common.js, common.jvm)

lazy val server = project
  .in(file("server"))
  .dependsOn(common.jvm, client)
  .enablePlugins(PlayScala)
  .settings (
    name := "boards-server",

    scalacOptions ++= Seq (
      "-feature",
      "-language:implicitConversions"
    ),
    javaOptions += "-Dplay.editor=http://localhost:63342/api/file/?file=%s&line=%s",
    
    libraryDependencies ++= Seq (
      "com.typesafe.slick" %% "slick" % "3.5.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1",
      "org.slf4j" % "slf4j-api" % "2.0.13",
      "org.playframework" %% "play-slick" % "6.1.0",
      "org.playframework" %% "play-slick-evolutions" % "6.1.0",
      "com.h2database" % "h2" % "2.2.224",
      guice
    ),
    dependencyOverrides += "org.slf4j" % "slf4j-api" % "2.0.13"
  )

lazy val client = project
  .in(file("client"))
  .dependsOn(common.js)
  .enablePlugins(ScalaJSPlugin)
  .settings (
    name := "boards-client",
    
    scalaVersion := "3.4.2",
    scalacOptions ++= Seq (
      "-feature",
      "-language:implicitConversions"
    ),
    
    libraryDependencies ++= Seq (
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.0.0",
      "com.raquo" %%% "airstream" % "17.0.0"
    )
  )

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("common"))
  .settings (
    name := "boards-common",
    
    scalaVersion := "3.4.2",
    scalacOptions ++= Seq (
      "-feature",
      "-language:implicitConversions"
    )
  )

val jsPath = "server/public/scripts"

lazy val jsCompileDev = taskKey[Unit]("")
jsCompileDev := {
  val compiled = (client / Compile / fastOptJS).value.data
  IO.copyFile (
    compiled,
    baseDirectory.value / jsPath / "application.js"
  )
  IO.copyFile (
    compiled.getParentFile / "boards-client-fastopt.js.map",
    baseDirectory.value / jsPath / "application.js.map"
  )
}

lazy val jsCompileProd = taskKey[Unit]("")
jsCompileProd := IO.copyFile(
  (client / Compile / fullOptJS).value.data,
  baseDirectory.value / jsPath / "application.js"
)

addCommandAlias("dev", "~ ;jsCompileDev ;server/run")
addCommandAlias("prod", ";jsCompileProd ;server/run")