package org.newtonpolyhedron.solve.matrixuni

import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.fraction.BigFraction
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.BigFrac

trait UnimodularMatrixMaker {
  /**
   * Makes an uniumodular matrix "alpha", that can be used in further Power Transformation
   * process.
   * <p>
   * For more info, see Bruno's "Power Geometry".
   *
   * @param matrix
   *            source matrix with arbitary determinant
   * @return unimodular "alpha"-matrix (i.e. {@code det(alpha) == 1})
   */
  def getUnimodularFrom(matrix: FieldMatrix[BigFraction]): FieldMatrix[BigFraction]

  /**
   * Makes an uniumodular matrix "alpha", that can be used in further Power Transformation
   * process.
   * <p>
   * For more info, see Bruno's "Power Geometry".
   *
   * @param matrix
   *            source matrix with arbitary determinant
   * @return unimodular "alpha"-matrix (i.e. {@code det(alpha) == 1})
   */
  def unimodularFrom(matrix: Matrix[BigFrac]): Matrix[BigFrac]
}