lazy val root = project.in(file(".root"))
  .aggregate(
    `deriving-scalac`,
    `deriving-dotty`
  )

lazy val `deriving-scalac` = project.in(file(".deriving-scalac"))
  .settings(
    scalaVersion := "2.12.98-bin-SNAPSHOT", // Published local scalac after #6050 and divergence disabled
                                            // Commit from my fork: 3d5b078a311b62147aed65677d193df33cd7b4ea
    scalaSource in Compile := baseDirectory.value / "../src/main/scala",
    scalaSource in Test    := baseDirectory.value / "../src/test/scala",
    scalacOptions := Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:experimental.macros",
      "-unchecked",
      // "-Xfatal-warnings",
      "-Xlint:-unused,_",
      // "-Xprint:typer",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-unused:imports,patvars,privates,locals",
      "-Xfuture"
    ),
    libraryDependencies := Seq(
      "com.chuusai" %% "shapeless" % "2.3.2",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    )
  )

lazy val `deriving-dotty` = project.in(file(".deriving-dotty"))
  .settings(
    scalaVersion := "0.4.0-RC1",
    scalaSource in Compile := baseDirectory.value / "../src/main/scala",
    scalaSource in Test    := baseDirectory.value / "../src/test/scala",
    scalacOptions := Seq(
      // "-Xprint:frontEnd",
      "-Xmin-implicit-search-depth", "32"
    )
  )

onLoad in Global ~= (_.andThen("project root" :: _))
