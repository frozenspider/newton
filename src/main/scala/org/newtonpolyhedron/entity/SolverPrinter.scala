package org.newtonpolyhedron.entity

import java.io.PrintWriter

/**
 * Launches the given solver, pretty-printing its output to a given writer.
 *
 * @author FS
 */
abstract class SolverPrinter[T](val solver: T, val output: PrintWriter) {

  def solveAndPrint: Unit = solveFor(solver, output)

  protected def solveFor(
    solver: T,
    output: PrintWriter
  ): Unit

  //
  // Print helpers
  //

  protected def title(title: String): String = {
    val bordered = "== " + title + " =="
    val line = "=" * bordered.length
    "\n\n\n" + line + "\n" + bordered + "\n" + line
  }

  protected def header(header: String): String =
    "\n=== " + header + " ==="

  protected def subheader(subheader: String): String =
    "\n" + subheader + ""
}
