package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.matrixminorgcd.MatrixMinorGCDSolver

class MatrixMinorGCDSolverPrinter[N <: MPNumber, M <: MPMatrix](
  override val solver: MatrixMinorGCDSolver[N, M],
  val baseMatrix:      M,
  override val output: PrintWriter
)
    extends SolverPrinter[MatrixMinorGCDSolver[N, M]](solver, output) {

  override def solveFor(
      solver: MatrixMinorGCDSolver[N, M],
      output: PrintWriter
  ) = {
    output.println(title("Matrix GCD"))
    val result = solver.lastRowGcd(baseMatrix)
    output.println(subheader("Base matrix:"))
    output.println(baseMatrix)
    output.println("Last row minors:")
    output.println(result._2 mkString " ")
    output.println("Minors GCD: ")
    output.println(result._1)
  }
}
