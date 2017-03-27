package org.newtonpolyhedron.utils

import scala.collection.GenTraversableLike
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.SortedSet

import spire.math.Rational
import spire.math.SafeLong

trait LanguageImplicits {
  // Mapping helpers
  implicit def funOfTwo2funOfMonad[A1, A2, T](f: (A1, A2) => T): ((A1, A2)) => T =
    p => f(p._1, p._2)

  implicit def funOfThree2funOfMonad[A1, A2, A3, T](f: (A1, A2, A3) => T): ((A1, A2, A3)) => T =
    p => f(p._1, p._2, p._3)

  implicit def set2sorted[T <: Ordered[T]](t: Set[T]): SortedSet[T] = {
    SortedSet.empty[T] ++ t
  }

  implicit class ExtBigInt(val n: BigInt) {
    def factorial: BigInt = {
      if (n == 0) BigInt(1)
      else if (n == 1) BigInt(1)
      else if (n > 1) (BigInt(2) to n) reduce (_ * _)
      else throw new IllegalArgumentException("Factorial of non-positive number " + n)
    }
  }

  implicit class ExtInt(val n: Int) {
    def factorial: Int = {
      val res = BigInt(n).factorial
      if (!res.isValidInt) throw new IllegalArgumentException(s"${n}! is too large")
      res.toInt
    }

    def ! : Int = ExtInt(n).factorial

    def choose(k: Int): Int =
      if (k == 0 || n == k) 1
      else {
        val num = (BigInt(n - k + 1) to BigInt(n)).foldLeft(BigInt(1))(_ * _)
        val den = BigInt(k).factorial
        assert(num % den == 0)
        val res = num / den
        if (!res.isValidInt) throw new IllegalArgumentException(s"($n $k) is too large")
        res.toInt
      }
  }

  implicit class ExtRational(val r: Rational) {
    def quotient: SafeLong = {
      r.numerator / r.denominator
    }

    def remainder: SafeLong = {
      r.numerator % r.denominator
    }
  }

  implicit class SuperSeq[T](trav: Seq[T]) {
    def splitBySkippingDelim(p: T => Boolean): IndexedSeq[IndexedSeq[T]] = {
      def splitByRec(t: Seq[T], localAcc: IndexedSeq[T], globalAcc: IndexedSeq[IndexedSeq[T]]): IndexedSeq[IndexedSeq[T]] = {
        if (t.isEmpty) {
          if (localAcc.isEmpty)
            globalAcc
          else
            globalAcc :+ localAcc
        } else {
          val (local2, global2) = {
            if (p(t.head)) {
              if (localAcc.isEmpty)
                (localAcc, globalAcc)
              else
                (IndexedSeq.empty, globalAcc :+ localAcc)
            } else {
              (localAcc :+ t.head, globalAcc)
            }
          }
          splitByRec(t.tail, local2, global2)
        }
      }
      splitByRec(trav, IndexedSeq.empty, IndexedSeq.empty)
    }
  }
}

object LanguageImplicits extends LanguageImplicits
