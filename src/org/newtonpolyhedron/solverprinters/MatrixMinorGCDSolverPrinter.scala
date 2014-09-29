package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.solve.matrixminorgcd.MatrixMinorGCDSolver

class MatrixMinorGCDSolverPrinter(solver: MatrixMinorGCDSolver,
                                  val baseMatrix: Matrix[BigFrac],
                                  output: PrintWriter)
    extends SolverPrinter[MatrixMinorGCDSolver](solver, output) {

  override def solveFor(solver: MatrixMinorGCDSolver,
                        output: PrintWriter) = {
    val result = solver.lastRowGcd(baseMatrix)
    output.println(subheader("Base matrix:"))
    output.println(baseMatrix)
    output.println("Last row minors:")
    output.println(result._2 mkString " ")
    output.println("Minors GCD: ")
    output.println(result._1)
  }
}
