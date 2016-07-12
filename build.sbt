scalaVersion := "2.11.8"

libraryDependencies := Seq(
  "com.chuusai"   %% "shapeless" % "2.3.1",
  "org.typelevel" %% "cats"      % "0.6.0")

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
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  // "-Xlog-implicits",
  "-Xfuture")
