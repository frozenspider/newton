package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter

class MatrixInverseSolverPrinter[N <: MPNumber](
  val baseMatrix:      Matrix[N],
  override val output: PrintWriter
)(implicit mp: MathProcessor[N])
    extends SolverPrinter[Void](null, output) {

  override def solveFor(
      nothing: Void,
      output:  PrintWriter
  ) = {
    output.println(title("Matrix inversion"))
    val inv = baseMatrix.inverse
    output.println(subheader("Base matrix:"))
    output.println(baseMatrix)
    output.println(subheader("Inverse matrix:"))
    output.println(inv)
  }
}
