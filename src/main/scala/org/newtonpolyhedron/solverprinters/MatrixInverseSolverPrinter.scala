package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.matrix.Matrix
import spire.math.Rational

class MatrixInverseSolverPrinter(
  val baseMatrix:      Matrix[Rational],
  override val output: PrintWriter
)
    extends SolverPrinter[Void](null, output) {

  override def solveFor(
      nothing: Void,
      output:  PrintWriter
  ) = {
    output.println(title("Matrix inversion"))
    val inv = baseMatrix.inv
    output.println(subheader("Base matrix:"))
    output.println(baseMatrix)
    output.println(subheader("Inverse matrix:"))
    output.println(inv)
  }
}
