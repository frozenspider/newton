package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import org.newtonpolyhedron.utils.StringUtils

class UnimodularMatrixMakerPrinter(solver: UnimodularMatrixMaker,
                                   val baseMatrix: Matrix[BigFrac],
                                   output: PrintWriter)
    extends SolverPrinter[UnimodularMatrixMaker](solver, output) {

  override def solveFor(solver: UnimodularMatrixMaker,
                        output: PrintWriter) = {
    val alpha = solver.unimodularFrom(baseMatrix)
    val alphaInv = alpha.inv
    output.println(title("Unimodular \"Alpha\" matrix"))
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
