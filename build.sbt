import jgogstad.sbt.Versions.V

ThisBuild / organization := "jgogstad"

lazy val `advent` = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"          %% "cats-effect"              % V.fp.CatsEffect,
      "co.fs2"                 %% "fs2-core"                 % V.fp.Fs2Core,
      "co.fs2"                 %% "fs2-io"                   % V.fp.Fs2Core,
      "org.typelevel"          %% "cats-core"                % V.fp.Cats,
      "org.typelevel"          %% "squants"                  % V.types.Squants,
      "org.scala-lang.modules" %% "scala-collection-contrib" % V.types.ScalaCollectionContrib,
      "org.typelevel"          %% "spire"                    % V.math.Spire,
      "org.jgrapht"             % "jgrapht-core"             % V.math.Jgrapht,
      "com.lihaoyi"            %% "fastparse"                % V.parsing.Fastparse,
      "org.scalanlp"           %% "breeze"                   % V.math.Breeze,
      "org.scodec"             %% "scodec-core"              % V.data.Scodec,
      "org.slf4j"               % "slf4j-simple"             % V.logging.Slf4J,
      "org.typelevel"          %% "log4cats-slf4j"           % V.logging.Log4cats
    ),
    scalacOptions -= "-Xfatal-warnings"
  )
