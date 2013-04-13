package org.newtonpolyhedron.entity.vector

import Ordering.Implicits._
import Numeric.Implicits._
import org.newtonpolyhedron.entity.BigFrac
import org.apache.commons.math3.fraction.BigFraction
import org.newtonpolyhedron.utils.BigFracFormat

class FracMathVec(elements: IndexedSeq[BigFrac]) extends MathVector[BigFrac, FracMathVec](elements) {

  override protected def create(elements: IndexedSeq[BigFrac]) = new FracMathVec(elements)

  def *(n: BigInt) =
    create(elements map (_ * n))

  def isZero =
    elements forall (_ == FracMathVec.zeroValue)
}

object FracMathVec {
  private val zeroValue = BigFrac(0, 1)

  implicit def create(elements: IndexedSeq[BigFrac]) = new FracMathVec(elements)

  def zero(dim: Int) = new FracMathVec(IndexedSeq.fill(dim)(zeroValue))

  def apply(elements: BigFrac*): FracMathVec = new FracMathVec(elements.toIndexedSeq)

  def fromInts(elements: Int*): FracMathVec = new FracMathVec((elements map BigFrac.apply).toIndexedSeq)

  object FracMathVecFormat extends VectorFormat[BigFrac, FracMathVec] {
    private val frFormat = new BigFracFormat

    def createArrayOfZeros(length: Int): Array[BigFrac] =
      Array.fill(length)(BigFrac.ZERO)

    def parseElement(src: String): BigFrac =
      frFormat.parse(src).asInstanceOf[BigFrac]

    def makeVector(components: Seq[BigFrac]): FracMathVec =
      FracMathVec(components: _*)
  }
}
