package org.newtonpolyhedron.entity.vector

import org.newtonpolyhedron.math.MPNumber
import org.newtonpolyhedron.math.MathProcessor
import org.newtonpolyhedron.utils.LanguageImplicits._

import spire.implicits._
import spire.math.Rational

import internal.SeqVectorSupport
import spire.math.SafeLong

trait VectorImports extends SeqVectorSupport {

  type IntVec = IndexedSeq[BigInt]
  type FracVec = IndexedSeq[Rational]
  type NumVec[N <: MPNumber] = IndexedSeq[N]

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

  object NumVec {
    def apply[N <: MPNumber](vs: N*) = IndexedSeq.apply[N](vs: _*)

    def zero[N <: MPNumber](dimension: Int)(implicit mp: MathProcessor[N]) = IndexedSeq.fill[N](dimension)(mp.zero)
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

  implicit class RichNumVec[N <: MPNumber](val seq: NumVec[N]) {
    def toTupleString: String =
      seq mkString ("(", ", ", ")")
  }
}

object VectorImports extends VectorImports
