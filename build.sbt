scalaVersion := "2.11.8"

libraryDependencies := Seq(
  // "shapeless" % "2.3.2-SNAPSHOT" is a published local build of
  // https://github.com/milessabin/shapeless/pull/616
  "com.chuusai"          %% "shapeless"  % "2.3.2-SNAPSHOT",
  "org.typelevel"        %% "cats"       % "0.6.0",
  "com.github.mpilquist" %% "simulacrum" % "0.7.0",
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
)

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
  "-Xfuture"
)
