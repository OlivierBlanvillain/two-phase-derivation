// To compile with Dotty

// scalaVersion := "0.3.0-RC2"

// scalacOptions := Seq(
//   "-Xmin-implicit-search-depth", "32"
// )


// To compile with scalac

scalaVersion := "2.12.3"

libraryDependencies := Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value)

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
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused:imports,patvars,privates,locals",
  "-Xfuture")
