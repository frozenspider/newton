package org.newtonpolyhedron.solverprinters
import java.io.PrintWriter

import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.SolverPrinter

class MatrixInverseSolverPrinter(val baseMatrix: Matrix[BigFrac],
                                 output: PrintWriter)
    extends SolverPrinter[Void](null, output) {

  def this(baseMatrix: FieldMatrix[BigFraction],
           output: PrintWriter) =
    this(Matrix(baseMatrix.getData map (_ map (BigFrac(_)))), output)

  override def solveFor(nothing: Void,
                        output: PrintWriter) = {
    val inv = baseMatrix.inv
    output.println(subheader("Base matrix:"))
    output.println(baseMatrix)
    output.println(subheader("Matrix inverse:"))
    output.println(inv)
  }
}