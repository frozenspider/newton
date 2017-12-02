package org.newtonpolyhedron.entity.math

import spire.math.Rational

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

  /** Raise a number to a specified power */
  def raise(x: N, y: N): N

  /** Extract the principal root of the given number */
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
  // Matrix operations
  //

  def matrix: MatrixMathProcessor[N]
}
