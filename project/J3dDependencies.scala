import sbt._

object J3dDependencies {

  /** Special configuration used for OS-specific SWT jars dependency management */
  lazy val J3dConfig = config("j3d-config")

  val j3dLibPaths: Map[String, Seq[String]] = Map(
    "win32" -> Seq("win", "win/x86"),
    "win64" -> Seq("win", "win/x86_64"),
  )

  val j3dCurrOsLibPaths: Seq[String] = (sys.props("os.name"), sys.props("os.arch")) match {
    // case ("Linux", _)                              => j3dLibPaths("linux")
    // case ("Mac OS X", "amd64" | "x86_64")          => j3dLibPaths("mac64")
    // case ("Mac OS X", _)                           => j3dLibPaths("mac32")
    case (os, "amd64") if os.startsWith("Windows") => j3dLibPaths("win64")
    case (os, _) if os.startsWith("Windows")       => j3dLibPaths("win32")
    case (os, arch)                                => Seq.empty // sys.error("Cannot obtain lib for OS '" + os + "' and architecture '" + arch + "'")
  }

}
