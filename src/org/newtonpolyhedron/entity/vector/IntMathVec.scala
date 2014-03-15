package org.newtonpolyhedron.entity.vector

import Ordering.Implicits._
import Numeric.Implicits._
import java.math.BigInteger
import org.newtonpolyhedron.entity.BigFrac

class IntMathVec(els: Seq[BigInt]) extends MathVector[BigInt, IntMathVec](els.toIndexedSeq) {
  
  override protected def create(elements: Seq[BigInt]) = new IntMathVec(elements.toIndexedSeq)

  def *(n: BigInt) =
    create(elements map (_ * n))

  /** Divided by GCD*/
  def reduced = {
    val gcd = elements reduceLeft (_ gcd _)
    if (gcd == 0 || gcd == 1) this
    else new IntMathVec(elements map (_ / gcd))
  }

  def isZero =
    elements forall (_ == IntMathVec.ZERO)
}

object IntMathVec {
  private val ZERO = BigInt(0)
  private val ONE = BigInt(1)

  def zero(dim: Int): IntMathVec = new IntMathVec(IndexedSeq.fill(dim)(ZERO))

  def apply(elements: Int*): IntMathVec = new IntMathVec(elements.toIndexedSeq map (x => BigInt(x)))

  def fromFrac(v: FracMathVec): IntMathVec = fromFrac(v.elements: _*)

  def fromFrac(elements: BigFrac*): IntMathVec = {
    val multiplier = elements.foldLeft(ONE)((m, el) =>
      if (m % el.den == 0) m else m * el.den)
    val ints = elements map (x => x.num * multiplier / x.den)
    new IntMathVec(ints.toIndexedSeq).reduced
  }

  object IntMathVecFormat extends VectorFormat[BigInt, IntMathVec] {
    def createArrayOfZeros(length: Int): Array[BigInt] =
      Array.fill(length)(ZERO)

    def parseElement(src: String): BigInt =
      BigInt(src)

    def makeVector(components: Seq[BigInt]): IntMathVec =
      new IntMathVec(components.toIndexedSeq)
  }
}