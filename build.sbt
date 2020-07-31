
/*
 build.sbt adapted from https://github.com/pbassiner/sbt-multi-project-example/blob/master/build.sbt
*/


name := "bwhc-query-service"
organization in ThisBuild := "de.bwhc"
scalaVersion in ThisBuild := "2.13.1"
version in ThisBuild := "1.0-SNAPSHOT"


//-----------------------------------------------------------------------------
// PROJECTS
//-----------------------------------------------------------------------------

lazy val global = project
  .in(file("."))
  .settings(
    settings,
    publish / skip := true
  )
  .aggregate(
     api,
     impl,
     bwhc_connector,
     fs_mtbfile_db
  )


lazy val api = project
  .settings(
    name := "query-service-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.play_json,
      dependencies.cats_core,
      dependencies.bwhc_utils,
      dependencies.bwhc_data_api
    )
  )


lazy val impl = project
  .settings(
    name := "query-service-impl",
    settings,
    libraryDependencies ++= Seq(
    )
  )
  .dependsOn(
    api
  )


lazy val bwhc_connector = project
  .settings(
    name := "bwhc-connector",
    settings,
    libraryDependencies ++= Seq(
      dependencies.play_ws_client,
      dependencies.play_ws_json,
      dependencies.scala_xml
    )
  )
  .dependsOn(
    impl
  )


lazy val fs_mtbfile_db = project
  .settings(
    name := "fs-mtbfile-db",
    settings,
    libraryDependencies ++= Seq(
    )
  )
  .dependsOn(
    impl
  )

/*
lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.logback
    ),
    publish / skip := true
  )
  .dependsOn(
    api,
    impl % "test",
    fs_repositories % "test",
  )
*/

//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest      = "org.scalatest"          %% "scalatest"               % "3.1.1" % "test"
    val slf4j          = "org.slf4j"              %  "slf4j-api"               % "1.7.26"
    val logback        = "ch.qos.logback"         %  "logback-classic"         % "1.0.13" % "test"
    val cats_core      = "org.typelevel"          %% "cats-core"               % "2.1.1"
    val play_json      = "com.typesafe.play"      %% "play-json"               % "2.8.0"
    val play_ws_client = "com.typesafe.play"      %% "play-ahc-ws-standalone"  % "2.1.2"
    val play_ws_json   = "com.typesafe.play"      %% "play-ws-standalone-json" % "2.1.2"
    val scala_xml      = "org.scala-lang.modules" %% "scala-xml"               % "2.0.0-M1"
    val bwhc_utils     = "de.bwhc"                %% "utils"                   % "1.0-SNAPSHOT"
    val bwhc_data_api  = "de.bwhc"                %% "data-entry-service-api"  % "1.0-SNAPSHOT"
  }


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


lazy val compilerOptions = Seq(
  "-encoding", "utf8",
  "-unchecked",
  "-feature",
//  "-language:existentials",
//  "-language:higherKinds",
//  "-language:implicitConversions",
  "-language:postfixOps",
  "-Xfatal-warnings",
  "-deprecation",
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
  )
)

