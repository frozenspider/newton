package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import org.newtonpolyhedron.utils.StringUtils

class UnimodularMatrixMakerPrinter[N <: MPNumber, M <: MPMatrix](
  override val solver: UnimodularMatrixMaker[N, M],
  val baseMatrix:      M,
  override val output: PrintWriter
)(implicit mp: MathProcessor[N, M])
    extends SolverPrinter[UnimodularMatrixMaker[N, M]](solver, output) {

  override def solveFor(
      solver: UnimodularMatrixMaker[N, M],
      output: PrintWriter
  ) = {
    val alpha = solver.unimodularFrom(baseMatrix)
    val alphaInv = alpha.inverse
    output.println(title("""Unimodular "Alpha" matrix"""))
    val text1 = new StringBuilder()
    text1 ++= subheader("Base matrix:") + "\n"
    text1 ++= baseMatrix + "\n"
    val text2 = new StringBuilder()
    text2 ++= subheader("Alpha-matrix:") + "\n"
    text2 ++= alpha + "\n"
    val text3 = new StringBuilder()
    text3 ++= subheader("Inverse alpha-matrix:") + "\n"
    text3 ++= alphaInv + "\n"
    output.println(StringUtils.appendToRight(5, text1, text2, text3))
  }
}
