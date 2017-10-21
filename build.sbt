name         := "newton"
version      := "2.3"
scalaVersion := "2.12.3"

sourceManaged            := baseDirectory.value / "src_managed"
sourceManaged in Compile := baseDirectory.value / "src_managed" / "main" / "scala"
sourceManaged in Test    := baseDirectory.value / "src_managed" / "test" / "scala"

lazy val root = (project in file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, buildInfoBuildNumber),
    buildInfoPackage := "org.newtonpolyhedron"
  )

resolvers ++= Seq(
  // com.github.*
  "jitpack" at "https://jitpack.io",
  // Java3D
  "TUDelft" at "http://simulation.tudelft.nl/maven/"
)

libraryDependencies ++= Seq(
  // Math
  "org.apache.commons"      %  "commons-math3"        % "3.6.1",
  "org.scilab.forge"        %  "jlatexmath"           % "1.0.6",
  "org.typelevel"           %% "spire"                % "0.14.1",
  // Visual
  "org.scala-lang.modules"  %% "scala-swing"          % "2.0.0",
  "java3d"                  %  "j3d-core"             % "1.5.1",
  "java3d"                  %  "j3d-core-utils"       % "1.5.1",
  "java3d"                  %  "vecmath"              % "1.5.1",
  // Other
  "org.apache.commons"      %  "commons-lang3"        % "3.5",
  "com.github.frozenspider" %% "fs-common-utils"      % "0.1.2",
  // Test
  "junit"                   %  "junit"                % "4.12"  % "test",
  "org.scalactic"           %% "scalactic"            % "3.0.4" % "test",
  "org.scalatest"           %% "scalatest"            % "3.0.4" % "test"
)
