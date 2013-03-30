package org.newtonpolyhedron.entity

import java.io.PrintWriter
import org.apache.commons.lang3.StringUtils

abstract class SolverPrinter[T](val solver: T, val output: PrintWriter) {
  def solveAndPrint: Unit = solveFor(solver, output)
  def solveFor(solver: T, output: PrintWriter)
}

/** Print helpers */
object SolverPrinter {
  def title(title: String): String = {
    val bordered = "======= " + title + " ======="
    val line = StringUtils.repeat("=", bordered.length())
    "\n\n\n" + line + "\n" + bordered + "\n" + line;
  }

  def header(header: String): String =
    "\n=== " + header + " ===";

  def subheader(subheader: String): String =
    "\n" + subheader + ""

}