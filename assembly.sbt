mainClass          in assembly := Some("org.newtonpolyhedron.NewtonEntry")
assemblyJarName    in assembly := name.value + "-" + version.value + "b" + buildInfoBuildNumber.value + ".jar"
assemblyOutputPath in assembly := file("./_build") / (assemblyJarName in assembly).value
