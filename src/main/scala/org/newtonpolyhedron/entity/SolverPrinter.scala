package org.newtonpolyhedron.entity

import java.io.PrintWriter

abstract class SolverPrinter[T](val solver: T, val output: PrintWriter) {

  def solveAndPrint: Unit = solveFor(solver, output)

  def solveFor(
      solver: T,
      output: PrintWriter
  ): Unit

  //
  // Print helpers
  //
  def title(title: String): String = {
    val bordered = "======= " + title + " ======="
    val line = "=" * bordered.length
    "\n\n\n" + line + "\n" + bordered + "\n" + line
  }

  def header(header: String): String =
    "\n=== " + header + " ==="

  def subheader(subheader: String): String =
    "\n" + subheader + ""
}
