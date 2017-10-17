package org.newtonpolyhedron.math

import spire.math.Rational
import org.newtonpolyhedron.math.MathImports.Matrix
import org.newtonpolyhedron.math.MathImports.MatrixTriple

/**
 * Mathematical processor capable of performing math operations
 * on some internal representation of numbers and numerical expressions.
 *
 * @author FS
 */
trait MathProcessor[N <: MPNumber] {
  def zero: N

  def one: N

  def isZero(x: N): Boolean

  /** Whether the number is a valid integer (of arbitrary size) */
  def isIntegral(x: N): Boolean

  /** Whether the number is a valid m/n fraction */
  def isRational(x: N): Boolean

  def compare(x: N, y: N): Int

  /** @return 0 for 0, otherwise -1 or 1 indicating on the leading sign */
  def signum(x: N): Int

  def abs(x: N): N

  def negate(x: N): N

  def inverse(x: N): N

  def add(x: N, y: N): N

  def subtract(x: N, y: N): N

  def multiply(x: N, y: N): N

  def divide(x: N, y: N): N

  def raise(x: N, y: N): N

  /** Principal root */
  def proot(x: N, y: N): N

  def fromDouble(x: Double): N

  def fromInt(x: Int): N

  def fromBigInt(x: BigInt): N

  def fromRational(x: Rational): N

  def toInt(x: N): Int

  def toLong(x: N): Long

  def toDouble(x: N): Double

  def toRational(x: N): Rational

  def toLatexString(x: N): String

  //
  // Advanced operations
  //

  /**
   * Converts the matrix to diagonal form.
   * <p>
   * Returns diagonal matrix alongside with row and column transformation matrices
   * @return (`DiagonalMatrix`, `RowTransformationsMatrix`, `ColumnTransformationsMatrix`)
   */
  def diagonalize(m: Matrix[N]): MatrixTriple[N]
}
