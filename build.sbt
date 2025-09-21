import xerial.sbt.Sonatype.sonatypeCentralHost

val ReleaseTag = """^release/([\d\.]+a?)$""".r

lazy val contributors = Seq(
  "pchlupacek" -> "Pavel Chlupáček"
  , "AdamChlupacek" -> "Adam Chlupáček"
  , "mrauilm" -> "Milan Raulim"
)

val fs2Version = "3.12.2"

lazy val commonSettings = Seq(
  organization := "com.spinoco",
  scalaVersion := "2.13.16",
  crossScalaVersions := Seq("2.12.20", "2.13.16"),
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps",
    "-Xfatal-warnings"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) => Seq(
      "-Yno-adapted-args",
      "-Ywarn-value-discard",
      "-Ywarn-unused-import"
    )
    case Some((2, 13)) => Seq(
      "-Wvalue-discard",
      "-Wunused:imports"
    )
    case _ => Seq.empty
  }),
  scalacOptions in (Compile, console) ~= {_.filterNot(opt => opt == "-Ywarn-unused-import" || opt == "-Wunused:imports")},
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  libraryDependencies ++= Seq(
    "co.fs2" %% "fs2-core" % fs2Version
    , "co.fs2" %% "fs2-io" % fs2Version
    , "com.spinoco" %% "protocol-mail" % "0.5.1"
    , "com.beetstra.jutf7" % "jutf7" % "1.0.0"
    , "org.scalacheck" %% "scalacheck" % "1.18.1" % "test"
  ),
  scmInfo := Some(ScmInfo(url("https://github.com/Spinoco/fs2-mail"), "git@github.com:Spinoco/fs2-mail.git")),
  homepage := None,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  initialCommands := s"""
   import fs2._
   import fs2.mail._
  """
) ++ testSettings ++ scaladocSettings ++ publishingSettings ++ releaseSettings

lazy val testSettings = Seq(
  parallelExecution in Test := false,
  testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
  publishArtifact in Test := true
)

lazy val scaladocSettings = Seq(
  scalacOptions in (Compile, doc) ++= Seq(
    "-doc-source-url", scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-implicits",
    "-implicits-show-all"
  ),
  scalacOptions in (Compile, doc) ~= { _ filterNot { _ == "-Xfatal-warnings" } },
  autoAPIMappings := true
)

lazy val publishingSettings = Seq(
  sonatypeCredentialHost := sonatypeCentralHost,
  publishTo := sonatypePublishToBundle.value,
  versionScheme := Some("early-semver"),
  organization := "com.spinoco",
  homepage := Some(url("https://github.com/spinoco/fs2-mail")),
  licenses := List("MIT" -> url("http://opensource.org/licenses/MIT")),
  developers := {
    for ((username, name) <- contributors) yield
      Developer(
        username,
        name,
        "",
        url(s"https://github.com/$username")
      )
  }.toList,
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/spinoco/fs2-mail"),
      "scm:git@github.com:spinoco/fs2-mail.git"
    )
  )
)

lazy val releaseSettings = Seq(
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value
)

lazy val `fs2-mail`=
  project.in(file("./"))
  .settings(commonSettings)
  .settings(
    name := "fs2-mail"
  )
