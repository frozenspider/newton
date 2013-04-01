package org.newtonpolyhedron.solve.matrixminorgcd

import java.math.BigInteger

import org.apache.commons.lang3.tuple.Pair
import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix

trait MatrixMinorGCDSolver {
  /**
   * Get the GCD of matrix minors for the last row.
   *
   * @param matrix
   *            base (non-square) matrix, [row][column], N-1 x N
   * @return a matrix gcd and minors
   * @throws SingularMatrixException
   *             if matrix is singular
   */
  def getLastRowGcd(matrix: FieldMatrix[BigFraction]): Pair[Integer, java.util.List[BigInteger]]

  /**
   * Get the GCD of matrix minors for the last row.
   *
   * @param matrix
   *            base (non-square) matrix, [row][column], N-1 x N
   * @return a matrix gcd and minors
   * @throws SingularMatrixException
   *             if matrix is singular
   */
  def lastRowGcd(matrix: Matrix[BigFrac]): (BigInt, Seq[BigInt])
}