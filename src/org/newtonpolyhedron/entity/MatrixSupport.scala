package org.newtonpolyhedron.entity

import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.utils.compatibility.FieldElementSupport._

object MatrixSupport {
  def fromFracs(seq: Seq[FracVec]): Matrix[BigFrac] =
    Matrix.fromVectors(seq)(bigFracFieldWrapper)

  def fromInts(seq: Seq[IntVec]): Matrix[BigInt] =
    Matrix.fromVectors(seq)(bigIntFieldWrapper)
}
