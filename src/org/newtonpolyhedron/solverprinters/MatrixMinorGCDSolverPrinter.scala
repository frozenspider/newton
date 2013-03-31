package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.matrixminorgcd.MatrixMinorGCDSolver

class MatrixMinorGCDSolverPrinter(solver: MatrixMinorGCDSolver,
                                  val baseMatrix: Matrix[BigFrac],
                                  output: PrintWriter)
    extends SolverPrinter[MatrixMinorGCDSolver](solver, output) {

  def this(solver: MatrixMinorGCDSolver,
           baseMatrix: FieldMatrix[BigFraction],
           output: PrintWriter) =
    this(solver, Matrix(baseMatrix.getData map (_ map (BigFrac(_)))), output)

  override def solveFor(solver: MatrixMinorGCDSolver,
                        output: PrintWriter) = {
    val result = solver.lastRowGcd(baseMatrix);
    output.println(subheader("Last row minors:"));
    output.println(result._1);
    output.println("Minors GCD: ");
    output.println(result._2 mkString " ");
  }
}