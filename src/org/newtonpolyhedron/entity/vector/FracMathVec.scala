package org.newtonpolyhedron.entity.vector

import Ordering.Implicits._
import Numeric.Implicits._
import org.newtonpolyhedron.entity.BigFrac
import org.apache.commons.math3.fraction.BigFraction

class FracMathVec(elements: IndexedSeq[BigFrac]) extends MathVector[BigFrac, FracMathVec](elements) {

  override protected def create(elements: IndexedSeq[BigFrac]) = new FracMathVec(elements)

  def *(n: BigInt) =
    create(elements map (_ * n))

  def isZero =
    elements forall (_ == FracMathVec.zeroValue)
}

object FracMathVec {
  private val zeroValue = BigFrac(0, 1)

  def zero(dim: Int) = new FracMathVec(IndexedSeq.fill(dim)(zeroValue))

  def apply(elements: BigFrac*): FracMathVec = new FracMathVec(elements.toIndexedSeq)
}