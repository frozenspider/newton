package org.newtonpolyhedron.entity.vector

import org.newtonpolyhedron.math.MathImports._
import org.newtonpolyhedron.utils.LanguageImplicits._

import org.newtonpolyhedron.entity.vector.internal.SeqVectorSupport

import spire.math.SafeLong

trait VectorImports extends SeqVectorSupport {

  type IntVec = IndexedSeq[BigInt]
  type NumVec[N <: MPNumber] = IndexedSeq[N]

  object IntVec {
    def apply(vs: BigInt*) = IndexedSeq.apply[BigInt](vs: _*)

    def zero(dimension: Int) = IndexedSeq.fill[BigInt](dimension)(0)

    def fromNumVec[N <: MPNumber](ns: NumVec[N])(implicit mp: MathProcessor[N]) = {
      fromNum(ns: _*)
    }

    def fromNum[N <: MPNumber](ns: N*)(implicit mp: MathProcessor[N]) = {
      val rs = ns map (_.toRational)
      val multiplier = rs.foldLeft(SafeLong.one)((m, el) => {
        if (m % el.denominator == 0) m else m * el.denominator
      })
      val ints = rs map (x => x.numerator * multiplier / x.denominator)
      ints.map(_.toBigInt).toIndexedSeq.reduced
    }
  }

  object NumVec {
    def apply[N <: MPNumber](vs: N*) = IndexedSeq.apply[N](vs: _*)

    def zero[N <: MPNumber](dimension: Int)(implicit mp: MathProcessor[N]) = IndexedSeq.fill[N](dimension)(mp.zero)
  }

  implicit lazy val intVecOrdering: Ordering[IntVec] =
    Ordering.Implicits.seqDerivedOrdering[IndexedSeq, BigInt](Ordering.BigInt)

  implicit def numVecOrdering[N <: MPNumber](implicit mp: MathProcessor[N]): Ordering[NumVec[N]] = {
    val ordering = spire.compat.ordering(mpNumberSupport)
    Ordering.Implicits.seqDerivedOrdering[IndexedSeq, N](ordering)
  }

  implicit class RichIntVec(val seq: IntVec) {
    /** Divided by GCD*/
    def reduced: IntVec = {
      val gcd = seq reduceLeft (_ gcd _)
      if (gcd == 0 || gcd == 1)
        seq
      else
        seq map (_ / gcd)
    }

    def toNumVec[N <: MPNumber](implicit mp: MathProcessor[N]): NumVec[N] =
      seq map mp.fromBigInt

    def toTupleString: String =
      seq mkString ("(", ", ", ")")
  }

  implicit class RichNumVec[N <: MPNumber](val x: NumVec[N]) {
    def compare(y: NumVec[N])(implicit mp: MathProcessor[N]): Int =
      (x lengthCompare y.length) match {
        case 0 => (x zip y).toStream map (mp.compare) find (_ != 0) getOrElse 0
        case x => x
      }

    def toTupleString: String =
      x mkString ("(", ", ", ")")
  }
}

object VectorImports extends VectorImports
