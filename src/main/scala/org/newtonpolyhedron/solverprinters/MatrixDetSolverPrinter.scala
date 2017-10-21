package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.matrix.Matrix
import spire.math.Rational

class MatrixDetSolverPrinter(
  val baseMatrix: Matrix[Rational],
  val skipRow:    Int,
  val skipCol:    Int,
  output:         PrintWriter
)
    extends SolverPrinter[Void](null, output) {

  override def solveFor(
      nothing: Void,
      output:  PrintWriter
  ) = {
    val det = baseMatrix.minor(skipRow, skipCol)
    output.println(subheader("Base matrix:"))
    output.println(baseMatrix)
    if (skipRow != -1 || skipCol != -1) {
      (skipRow, skipCol) match {
        case (-1, -1) => // NOOP
        case (r, -1)  => output.println("(Skipping row " + skipRow + ")")
        case (-1, c)  => output.println("(Skipping col " + skipCol + ")")
        case (r, c)   => output.println("(Skipping row " + skipRow + ", col " + skipCol + ")")
      }
    }
    output.println("Matrix determinant: " + det)
  }
}
