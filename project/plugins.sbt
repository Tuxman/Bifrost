// Comment to get more information during initialization
logLevel := Level.Error

addDependencyTreePlugin

Seq(
  "com.eed3si9n"            % "sbt-assembly"              % "2.1.3",
  "org.scalastyle"         %% "scalastyle-sbt-plugin"     % "1.0.0",
  "org.scoverage"           % "sbt-scoverage"             % "2.0.9",
  "com.github.sbt"          % "sbt-release"               % "1.1.0",
  "io.kamon"                % "sbt-kanela-runner"         % "2.0.14",
  "com.github.cb372"        % "sbt-explicit-dependencies" % "0.3.1",
  "pl.project13.scala"      % "sbt-jmh"                   % "0.4.6",
  "org.scalameta"           % "sbt-scalafmt"              % "2.5.2",
  "ch.epfl.scala"           % "sbt-scalafix"              % "0.11.1",
  "org.wartremover"         % "sbt-wartremover"           % "3.1.4",
  "com.github.sbt"          % "sbt-native-packager"       % "1.9.16",
  "com.eed3si9n"            % "sbt-buildinfo"             % "0.11.0",
  "com.github.sbt"          % "sbt-ci-release"            % "1.5.12",
  "net.bzzt"                % "sbt-reproducible-builds"   % "0.31"
).map(addSbtPlugin)

// See: https://github.com/sbt/sbt/issues/6997
ThisBuild / libraryDependencySchemes ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
