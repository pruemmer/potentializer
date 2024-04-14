val scala3Version = "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "potentializer",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    resolvers += "uuverifiers" at "https://eldarica.org/maven/",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += ("uuverifiers" %% "princess" % "nightly-SNAPSHOT").cross(CrossVersion.for3Use2_13),
    libraryDependencies += "org.sat4j" % "org.sat4j.core" % "2.3.1"
  )
