package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.NewtonImports._

trait UnimodularMatrixMaker[N <: MPNumber] {
  /**
   * Makes an uniumodular matrix "alpha", that can be used in further Power Transformation
   * process.
   *
   * For more info, see Bruno's "Power Geometry".
   *
   * @param matrix
   *            source matrix with arbitary determinant
   * @return unimodular "alpha"-matrix (i.e. `det(alpha) == 1`)
   */
  def unimodularFrom(matrix: Matrix[N]): Matrix[N]
}
