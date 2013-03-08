package org.newtonpolyhedron.entity.vector

import Ordering.Implicits._
import Numeric.Implicits._

class IntMathVec(elements: IndexedSeq[BigInt]) extends MathVector[BigInt, IntMathVec](elements) {

  override protected def create(elements: IndexedSeq[BigInt]) = new IntMathVec(elements)

  def *(n: BigInt) =
    create(elements map (_ * n))

  /** Divided by GCD*/
  def reduced = {
    val gcd = elements reduceLeft (_ gcd _)
    if (gcd == 0 || gcd == 1) this
    else new IntMathVec(elements map (_ / gcd))
  }

  def isZero =
    elements forall (_ == IntMathVec.zeroValue)
}

object IntMathVec {
  private val zeroValue = BigInt(0)

  def zero(dim: Int) = new IntMathVec(Vector.fill(dim)(zeroValue))

  def apply(elements: Int*): IntMathVec = new IntMathVec(elements.toIndexedSeq map (x => BigInt(x)))
}