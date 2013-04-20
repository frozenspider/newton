package org.newtonpolyhedron.entity

import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec

object MatrixSupport {

  def fromFracs(seq: Seq[FracMathVec]): Matrix[BigFrac] =
    Matrix[BigFrac, FracMathVec](seq)

  def fromInts(seq: Seq[IntMathVec]): Matrix[BigIntFielded] =
    Matrix(seq)
}