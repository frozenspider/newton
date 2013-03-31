package org.newtonpolyhedron.solverprinters
import java.io.PrintWriter

import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.SolverPrinter

class MatrixDetSolverPrinter(val baseMatrix: Matrix[BigFrac],
                             val skipRow: Int,
                             val skipCol: Int,
                             output: PrintWriter)
    extends SolverPrinter[Void](null, output) {

  def this(baseMatrix: FieldMatrix[BigFraction],
           skipRow: Int,
           skipCol: Int,
           output: PrintWriter) =
    this(Matrix(baseMatrix.getData map (_ map (BigFrac(_)))), skipRow, skipCol, output)

  override def solveFor(nothing: Void,
                        output: PrintWriter) = {
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