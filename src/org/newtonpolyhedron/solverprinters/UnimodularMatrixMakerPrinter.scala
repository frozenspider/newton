package org.newtonpolyhedron.solverprinters
import java.io.PrintWriter

import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import org.newtonpolyhedron.utils.StringUtils

class UnimodularMatrixMakerPrinter(solver: UnimodularMatrixMaker,
                                   val baseMatrix: Matrix[BigFrac],
                                   output: PrintWriter)
    extends SolverPrinter[UnimodularMatrixMaker](solver, output) {

  def this(solver: UnimodularMatrixMaker,
           baseMatrix: FieldMatrix[BigFraction],
           output: PrintWriter) =
    this(solver, Matrix(baseMatrix.getData map (_ map (BigFrac(_)))), output)

  override def solveFor(solver: UnimodularMatrixMaker,
                        output: PrintWriter) = {
    val alpha = solver.unimodularFrom(baseMatrix);
    val alphaInv = alpha.inv
    output.println(title("Unimodular \"Alpha\" matrix"));
    val text1 = new StringBuilder();
    text1.append(subheader("Base matrix:") + "\n");
    text1.append(baseMatrix + "\n");
    val text2 = new StringBuilder();
    text2.append(subheader("Alpha-matrix:") + "\n");
    text2.append(alpha + "\n");
    val text3 = new StringBuilder();
    text3.append(subheader("Inverse alpha-matrix:") + "\n");
    text3.append(alphaInv + "\n");
    output.println(StringUtils.appendToRight(5, text1, text2, text3));
  }
}