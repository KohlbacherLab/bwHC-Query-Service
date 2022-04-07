
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
//     bwhc_broker_connector,
     fs_mtbfile_db,
     tests
  )


lazy val api = project
  .settings(
    name := "query-service-api",
    settings,
    libraryDependencies ++= Seq(
      dependencies.bwhc_data_api
    )
  )


lazy val impl = project
  .settings(
    name := "query-service-impl",
    settings,
    libraryDependencies ++= Seq(
      dependencies.hgnc_catalog_api,
      dependencies.icd_catalogs_api,
      dependencies.med_catalog_api 
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
      dependencies.scalatest,
      dependencies.play_ws_client,
      dependencies.play_ws_json,
      dependencies.scala_xml
    )
  )
  .dependsOn(
    impl
  )

/*
lazy val bwhc_broker_connector = project
  .settings(
    name := "bwhc-broker-connector",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.play_ws_client,
      dependencies.play_ws_json,
      dependencies.scala_xml
    )
  )
  .dependsOn(
    impl
  )
*/


lazy val fs_mtbfile_db = project
  .settings(
    name := "fs-mtbfile-db",
    settings,
    libraryDependencies ++= Seq(
      dependencies.bwhc_dto_gens
    )
  )
  .dependsOn(
    impl
  )

lazy val tests = project
  .settings(
    name := "tests",
    settings,
    libraryDependencies ++= Seq(
      dependencies.scalatest,
      dependencies.hgnc_catalog_impl,
      dependencies.icd_catalogs_impl,
      dependencies.med_catalog_impl 
    ),
    publish / skip := true
  )
  .dependsOn(
    api,
    impl % Test,
    fs_mtbfile_db % Test
  )


//-----------------------------------------------------------------------------
// DEPENDENCIES
//-----------------------------------------------------------------------------

lazy val dependencies =
  new {
    val scalatest          = "org.scalatest"          %% "scalatest"               % "3.1.1" % Test
    val slf4j              = "org.slf4j"              %  "slf4j-api"               % "1.7.32"
//    val logback            = "ch.qos.logback"         %  "logback-classic"         % "1.0.13" % Test
    val play_ws_client     = "com.typesafe.play"      %% "play-ahc-ws-standalone"  % "2.1.2"
    val play_ws_json       = "com.typesafe.play"      %% "play-ws-standalone-json" % "2.1.2"
    val scala_xml          = "org.scala-lang.modules" %% "scala-xml"               % "2.0.0"
    val bwhc_utils         = "de.bwhc"                %% "utils"                   % "1.0-SNAPSHOT"
    val bwhc_data_api      = "de.bwhc"                %% "data-entry-service-api"  % "1.0-SNAPSHOT"
    val bwhc_dto_gens      = "de.bwhc"                %% "mtb-dto-generators"      % "1.0-SNAPSHOT"
    val hgnc_catalog_api   = "de.bwhc"                %% "hgnc-api"                % "1.0-SNAPSHOT"
    val icd_catalogs_api   = "de.bwhc"                %% "icd-catalogs-api"        % "1.0-SNAPSHOT"
    val med_catalog_api    = "de.bwhc"                %% "medication-catalog-api"  % "1.0-SNAPSHOT"
    val hgnc_catalog_impl  = "de.bwhc"                %% "hgnc-impl"               % "1.0-SNAPSHOT" % Test
    val icd_catalogs_impl  = "de.bwhc"                %% "icd-catalogs-impl"       % "1.0-SNAPSHOT" % Test
    val med_catalog_impl   = "de.bwhc"                %% "medication-catalog-impl" % "1.0-SNAPSHOT" % Test
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

