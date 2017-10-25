package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.NewtonImports._

trait UnimodularMatrixMaker[N <: MPNumber, M <: MPMatrix] {

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
  def unimodularFrom(matrix: M): M
}
