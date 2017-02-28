package org.newtonpolyhedron.entity.vector

import org.newtonpolyhedron.utils.LanguageImplicits._

import spire.implicits._
import spire.math.Rational

import internal.SeqVectorSupport
import spire.math.SafeLong

object VectorImports extends SeqVectorSupport {

  type IntVec = IndexedSeq[BigInt]
  type FracVec = IndexedSeq[Rational]

  object IntVec {
    def apply(vs: BigInt*) = IndexedSeq.apply[BigInt](vs: _*)

    def zero(dimension: Int) = IndexedSeq.fill[BigInt](dimension)(0)

    def fromFracVec(fs: FracVec) = {
      fromFrac(fs: _*)
    }

    def fromFrac(fs: Rational*) = {
      val multiplier = fs.foldLeft(SafeLong.one)((m, el) =>
        if (m % el.denominator == 0) m else m * el.denominator)
      val ints = fs map (x => x.numerator * multiplier / x.denominator)
      ints.map(_.toBigInt).toIndexedSeq.reduced
    }
  }

  object FracVec {
    def apply(vs: Rational*) = IndexedSeq.apply[Rational](vs: _*)

    def zero(dimension: Int) = IndexedSeq.fill[Rational](dimension)(0)
  }

  implicit lazy val intVecOrdering: Ordering[IntVec] = Ordering.Implicits.seqDerivedOrdering
  implicit lazy val fracVecOrdering: Ordering[FracVec] = Ordering.Implicits.seqDerivedOrdering

  implicit class RichIntVec(val seq: IntVec) {
    /** Divided by GCD*/
    def reduced: IntVec = {
      val gcd = seq reduceLeft (_ gcd _)
      if (gcd == 0 || gcd == 1)
        seq
      else
        seq map (_ / gcd)
    }

    def toFracVec: FracVec =
      seq map Rational.apply

    def toTupleString: String =
      seq mkString ("(", ", ", ")")
  }

  implicit class RichFracVec(val seq: FracVec) {
    def toTupleString: String =
      seq mkString ("(", ", ", ")")
  }
}
