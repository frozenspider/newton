package org
import java.math.BigInteger
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.collection.JavaConversions._
import org.newtonpolyhedron.entity.BigIntFielded
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.vector._
import scala.collection.immutable.SortedSet
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.BigIntFielded
import scala.collection.TraversableLike
import scala.collection.SeqLike

package object newtonpolyhedron {

  type Polynomial = IndexedSeq[Term]
  type Polys = IndexedSeq[Polynomial]

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
}