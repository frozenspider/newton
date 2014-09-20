package org.newtonpolyhedron.entity

import org.newtonpolyhedron.entity.vector.VectorImports._

object MatrixSupport {

  def fromFracs(seq: Seq[FracVec]): Matrix[BigFrac] =
    Matrix.fromVectors[BigFrac](seq)

  def fromInts(seq: Seq[IntVec]): Matrix[BigIntFielded] =
    Matrix.fromIntVectors(seq)
}
