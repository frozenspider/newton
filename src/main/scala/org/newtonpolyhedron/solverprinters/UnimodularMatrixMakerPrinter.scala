package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import org.newtonpolyhedron.utils.StringUtils

class UnimodularMatrixMakerPrinter[N <: MPNumber](
  override val solver: UnimodularMatrixMaker[N],
  val baseMatrix:      Matrix[N],
  override val output: PrintWriter
)(implicit mp: MathProcessor[N])
    extends SolverPrinter[UnimodularMatrixMaker[N]](solver, output) {

  override def solveFor(
      solver: UnimodularMatrixMaker[N],
      output: PrintWriter
  ) = {
    val alpha = solver.unimodularFrom(baseMatrix)
    val alphaInv = alpha.inverse
    // Using .toString as a workaround for https://github.com/scala/bug/issues/10573
    output.println(title("""Unimodular "Alpha" matrix"""))
    val text1 = new StringBuilder()
    text1 ++= subheader("Base matrix:") + "\n"
    text1 ++= baseMatrix.toString + "\n"
    val text2 = new StringBuilder()
    text2 ++= subheader("Alpha-matrix:") + "\n"
    text2 ++= alpha.toString + "\n"
    val text3 = new StringBuilder()
    text3 ++= subheader("Inverse alpha-matrix:") + "\n"
    text3 ++= alphaInv.toString + "\n"
    output.println(StringUtils.appendToRight(5, text1, text2, text3))
  }
}
