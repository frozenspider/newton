package org.newtonpolyhedron.entity.vector.internal

import spire.implicits._
import spire.math.Numeric

private[vector] trait SeqVectorSupport {

  /**
   * Adds support for mathematical vector operations to indexed sequences, allowing
   * to effectively use them to representing vectors in the application.
   */
  implicit class MathSeqVector[T](val seq: IndexedSeq[T])(implicit numeric: Numeric[T]) {

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

    def /(that: T): IndexedSeq[T] = {
      seq map (_ / that)
    }

    //    def *(that: T): IndexedSeq[T] = {
    //      require(seq.size == that.size, "Dimension of other vector was different")
    //      (seq zip that) map { case (a, b) => numeric.times(a, b) }
    //    }

    /** Dot-product */
    def *+(that: Seq[T]) = {
      require(seq.size == that.size, "Dimension of other vector was different")
      (this * that).sum(spire.compat.numeric[T])
    }

    def isZero: Boolean = {
      seq forall (_ == numeric.zero)
    }

    /** Serves as "updated" method, but doesn't screw types and allows implicits to work */
    def upd(i: Int, v: T): IndexedSeq[T] = {
      seq.updated(i, v)
    }
  }
}
