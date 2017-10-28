package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter

class MatrixDetSolverPrinter[N <: MPNumber](
  val baseMatrix: Matrix[N],
  val skipRow:    Int,
  val skipCol:    Int,
  override val output:         PrintWriter
)(implicit mp: MathProcessor[N])
    extends SolverPrinter[Void](null, output) {

  override def solveFor(
      nothing: Void,
      output:  PrintWriter
  ) = {
    output.println(title("Matrix determinant"))
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
    output.println("Determinant: " + det)
  }
}
