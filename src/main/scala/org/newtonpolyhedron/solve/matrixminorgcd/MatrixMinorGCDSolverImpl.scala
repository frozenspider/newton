package org.newtonpolyhedron.solve.matrixminorgcd

import org.newtonpolyhedron.NewtonImports._

class MatrixMinorGCDSolverImpl[N <: MPNumber](implicit mp: MathProcessor[N])
    extends MatrixMinorGCDSolver[N] {
  override def lastRowGcd(matrix: Matrix[N]): (BigInt, Seq[BigInt]) = {
    require(matrix.isSquare, "Matrix must be square (although last row is zeros)")
    val dets = for (skipCol <- 0 until matrix.colCount) yield {
      val minor = matrix.minorMatrix(matrix.rowCount - 1, skipCol).det
      assert(minor.isRational, "Irrational minor")
      val minorRat = minor.toRational
      assert(minorRat.isWhole, "Non-integer minor")
      minorRat.numerator
    }
    val gcd = dets.reduceLeft(_ gcd _)
    (gcd.toBigInt, dets map (_.toBigInt))
  }
}
