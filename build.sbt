lazy val root = (project in file("."))
  .aggregate(server, client, common)

lazy val server = project.dependsOn(common).in(file("server"))
  .enablePlugins(PlayScala)
  .settings (
    name := "boards-server",
    version := "4.0-beta",
    organization := "swagco",
    
    scalaVersion := "3.4.2",
    scalacOptions ++= Seq (
      "-feature"
    ),
    
    libraryDependencies ++= Seq (
      "com.typesafe.slick" %% "slick" % "3.5.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1",
      "org.slf4j" % "slf4j-nop" % "2.0.13"
    )
  )

lazy val client = project.dependsOn(common).in(file("client"))
  .enablePlugins(ScalaJSPlugin)
  .settings (
    name := "boards-client",
    version := "4.0-beta",
    organization := "swagco",
    
    scalaVersion := "3.4.2",
    scalacOptions ++= Seq (
      "-feature"
    ),
    
    libraryDependencies ++= Seq (
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "com.raquo" %%% "laminar" % "17.0.0",
      "com.raquo" %%% "airstream" % "17.0.0"
    )
  )

lazy val common = project.in(file("common"))
  .settings (
    name := "boards-common",
    version := "4.0-beta",
    organization := "swagco",
    
    scalaVersion := "3.4.2",
    scalacOptions ++= Seq (
      "-feature"
    ),
    
    libraryDependencies ++= Seq (
    
    )
  )