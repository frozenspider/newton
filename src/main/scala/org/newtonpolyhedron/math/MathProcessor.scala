package org.newtonpolyhedron.math

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

  def compare(x: N, y: N): Int

  def negate(x: N): N

  def inverse(x: N): N

  def add(x: N, y: N): N

  def subtract(x: N, y: N): N

  def multiply(x: N, y: N): N

  def divide(x: N, y: N): N

  def raise(x: N, y: N): N

  /** Principal root */
  def proot(x: N, y: N): N

  def fromInt(x: Int): N

  def fromBigInt(x: BigInt): N

  def fromRational(x: Rational): N

  def toInt(x: N): Int

  def toLong(x: N): Long

  def toDouble(x: N): Double
}
