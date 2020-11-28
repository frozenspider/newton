//
// sbt-assembly configuration
//

val buildOutputPath = file("./_build")

mainClass          in assembly := Some("org.newtonpolyhedron.NewtonEntry")
assemblyJarName    in assembly := name.value + "-" + version.value + "b" + buildInfoBuildNumber.value + ".jar"
assemblyOutputPath in assembly := file("./_build") / (assemblyJarName in assembly).value

//
// copyLibs task
//

import J3dDependencies._

val libOutputDir = buildOutputPath

val copyLibs = taskKey[Unit](s"Copies all OS-specific libraries to ${libOutputDir}")

copyLibs := {
  val toCopy = new collection.mutable.HashSet[(File, File)]
  val paths = j3dCurrOsLibPaths map (unmanagedBase.value / _)
  paths.foreach { p =>
    p.listFiles().filter(!_.isDirectory).foreach { f =>
      toCopy += (f -> (buildOutputPath / f.getName))
    }
  }
  IO.copy(toCopy)
}

//
// buildDistr task
//

val buildDistr = taskKey[Unit](s"Complete build: assemble a runnable .jar, copy libs")

buildDistr := {
  assembly.value
  copyLibs.value
}
