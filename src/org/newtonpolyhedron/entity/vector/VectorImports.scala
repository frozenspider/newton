package org.newtonpolyhedron.entity.vector

import org.newtonpolyhedron.entity.BigFrac

object VectorImports extends SeqVectorSupport {
  type IntVec = IndexedSeq[BigInt]
  type FracVec = IndexedSeq[BigFrac]

  object IntVec {
    def apply(vs: BigInt*) = IndexedSeq.apply[BigInt](vs: _*)

    def zero(dimension: Int) = IndexedSeq.fill[BigInt](dimension)(0)

    def fromFracVec(fs: FracVec) = {
      fromFrac(fs: _*)
    }

    def fromFrac(fs: BigFrac*) = {
      val one = BigInt(1)
      val multiplier = fs.foldLeft(one)((m, el) =>
        if (m % el.den == 0) m else m * el.den)
      val ints = fs map (x => x.num * multiplier / x.den)
      ints.toIndexedSeq.reduced
    }
  }

  object FracVec {
    def apply(vs: BigFrac*) = IndexedSeq.apply[BigFrac](vs: _*)

    def zero(dimension: Int) = IndexedSeq.fill[BigFrac](dimension)(0)
  }

  implicit lazy val intVecOrdering: Ordering[IntVec] = Ordering.Implicits.seqDerivedOrdering
  implicit lazy val fracVecOrdering: Ordering[FracVec] = Ordering.Implicits.seqDerivedOrdering
}
