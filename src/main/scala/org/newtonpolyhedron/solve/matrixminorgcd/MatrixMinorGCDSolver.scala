package org.newtonpolyhedron.solve.matrixminorgcd

import org.newtonpolyhedron.NewtonImports._

trait MatrixMinorGCDSolver[N <: MPNumber] {
  /**
   * Get the GCD of matrix minors for the last row.
   *
   * @param matrix
   *            base (non-square) matrix, [row][column], N-1 x N
   * @return a matrix gcd and minors
   * @throws SingularMatrixException
   *             if matrix is singular
   */
  def lastRowGcd(matrix: Matrix[N]): (BigInt, Seq[BigInt])
}
