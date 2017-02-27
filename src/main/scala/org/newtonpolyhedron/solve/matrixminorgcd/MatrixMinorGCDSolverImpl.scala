package org.newtonpolyhedron.solve.matrixminorgcd

import org.newtonpolyhedron.entity.matrix.Matrix
import spire.math.Rational

class MatrixMinorGCDSolverImpl extends MatrixMinorGCDSolver {
  override def lastRowGcd(matrix: Matrix[Rational]): (BigInt, Seq[BigInt]) = {
    require(matrix.isSquare, "Matrix must be square (although last row is zeros)")
    val dets = for (skipCol <- 0 until matrix.colCount) yield {
      val minor = matrix.minorMatrix(matrix.rowCount - 1, skipCol).det
      assert(minor.isWhole, "Non-integer minor")
      minor.numerator
    }
    val gcd = dets.reduceLeft(_ gcd _)
    (gcd.toBigInt, dets map (_.toBigInt))
  }
}
