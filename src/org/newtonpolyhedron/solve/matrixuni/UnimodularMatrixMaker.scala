package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix

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
  def unimodularFrom(matrix: Matrix[BigFrac]): Matrix[BigFrac]
}