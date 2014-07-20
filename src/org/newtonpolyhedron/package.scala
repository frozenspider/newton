package org

import java.math.BigInteger
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.collection.JavaConversions._
import org.newtonpolyhedron.entity._
import org.newtonpolyhedron.entity.equation._
import org.newtonpolyhedron.entity.vector._
import scala.collection.immutable.SortedSet
import scala.collection.TraversableLike
import scala.collection.SeqLike
import scala.collection.IterableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.GenTraversableLike

package object newtonpolyhedron {

  type Polynomial = IndexedSeq[Term]
  type Polys = IndexedSeq[Polynomial]
  type Equations = IndexedSeq[Equation]

  def zeroPoly(dim: Int): Polynomial =
    IndexedSeq(Term(Product.ZERO, FracMathVec.zero(dim)))

  implicit class ExtPoly(val p: Polynomial) {
    def toPlainString: String = {
      p map (term => "(" +
        term.coeff.fracValue +
        ") * " +
        term.powers.elements.mapWithIndex((pow, i) => s"x${i + 1}^($pow)").mkString(" * ")
      ) mkString ("", " + ", " = 0")
    }
  }

  implicit class ExtInt(val n: Int) extends AnyVal {
    def factorial: Int = {
      val res = BigIntFielded(2).factorial
      if (!res.isValidInt) throw new IllegalArgumentException(s"${n}! is too large")
      res.toInt
    }

    def !(): Int = ExtInt(n).factorial

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

  implicit class SuperIterable[A, Repr](iter: IterableLike[A, Repr]) {
    def mapWithIndex[B, Repr2 <: GenTraversableLike[(A, Int), Repr2], That2](f: (A, Int) => B)(
      implicit bf1: CanBuildFrom[Repr, (A, Int), Repr2], bf2: CanBuildFrom[Repr2, B, That2]): That2 =
      {
        iter.zipWithIndex map (x => f(x._1, x._2))
      }

    def eachWithIndex[U, That <: GenTraversableLike[(A, Int), _]](f: (A, Int) => U)(
      implicit bf1: CanBuildFrom[Repr, (A, Int), That]): Unit =
      {
        iter.zipWithIndex foreach (x => f(x._1, x._2))
      }
  }

  implicit class OptionsIterable[A, Repr](iter: IterableLike[Option[A], Repr]) {
    def yieldDefined[Repr2 <: IterableLike[A, Repr2]](implicit bf: CanBuildFrom[Repr, A, Repr2]): Repr2 = {
      for (o <- iter if o.isDefined) yield o.get
    }
  }

  implicit def set2sorted[T <: Ordered[T]](t: Set[T]): SortedSet[T] = {
    SortedSet.empty[T] ++ t
  }

  // BigInt conversion
  implicit def int2Fielded(bigInt: BigInt): BigIntFielded =
    new BigIntFielded(bigInt.underlying)

  implicit def int2Fielded(bigInt: BigInteger): BigIntFielded =
    new BigIntFielded(bigInt)

  implicit def int2Fielded(int: Int): BigIntFielded =
    BigIntFielded(int)

  // Mapping helpers
  implicit def funOfTwo2funOfMonad[A1, A2, T](f: (A1, A2) => T): ((A1, A2)) => T =
    p => f(p._1, p._2)

  implicit def funOfThree2funOfMonad[A1, A2, A3, T](f: (A1, A2, A3) => T): ((A1, A2, A3)) => T =
    p => f(p._1, p._2, p._3)
}