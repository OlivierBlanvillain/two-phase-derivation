scalaVersion := "2.11.8"

libraryDependencies := Seq(
  "com.github.mpilquist" %% "simulacrum" % "0.7.0",
  "org.typelevel" %% "cats" % "0.4.1",
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"),
  compilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.1.0" cross CrossVersion.full))

scalacOptions := Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import",
  "-Xfuture")

sourceGenerators in Compile <+= (sourceManaged in Compile).map(Boilerplate.gen)
