package org.newtonpolyhedron.entity.vector

import org.newtonpolyhedron.entity.BigFrac

trait SeqVectorSupport {

  implicit class MathSeqVector[T](val seq: IndexedSeq[T])(implicit numeric: Numeric[T]) {

    import numeric._

    def +(that: Seq[T]): IndexedSeq[T] = {
      require(seq.size == that.size, "Dimension of other vector was different")
      (seq zip that) map { case (a, b) => a + b }
    }

    def -(that: Seq[T]): IndexedSeq[T] = {
      require(seq.size == that.size, "Dimension of other vector was different")
      (seq zip that) map { case (a, b) => a - b }
    }

    def unary_- : IndexedSeq[T] = {
      seq map (x => -x)
    }

    def *(that: Seq[T]): IndexedSeq[T] = {
      require(seq.size == that.size, "Dimension of other vector was different")
      (seq zip that) map { case (a, b) => a * b }
    }

    def *(that: T): IndexedSeq[T] = {
      seq map (_ * that)
    }

    //    def *(that: T): IndexedSeq[T] = {
    //      require(seq.size == that.size, "Dimension of other vector was different")
    //      (seq zip that) map { case (a, b) => numeric.times(a, b) }
    //    }

    /** Dot-product */
    def *+(that: Seq[T]) = {
      require(seq.size == that.size, "Dimension of other vector was different")
      (this * that).sum
    }

    def isZero: Boolean = {
      seq forall (_ == zero)
    }

    /** Serves as "updated" method, but doesn't screw types and allows implicits to work */
    def upd(i: Int, v: T): IndexedSeq[T] = {
      seq.updated(i, v)
    }
  }

  implicit class RichIntVec(val seq: IndexedSeq[BigInt]) {
    /** Divided by GCD*/
    def reduced: IndexedSeq[BigInt] = {
      val gcd = seq reduceLeft (_ gcd _)
      if (gcd == 0 || gcd == 1)
        seq
      else
        seq map (_ / gcd)
    }
  }
}
