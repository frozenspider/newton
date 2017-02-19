package org.newtonpolyhedron.solve.matrixminorgcd

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.matrix.Matrix

class MatrixMinorGCDSolverImpl extends MatrixMinorGCDSolver {
  override def lastRowGcd(matrix: Matrix[BigFrac]): (BigInt, Seq[BigInt]) = {
    require(matrix.isSquare, "Matrix must be square (although last row is zeros)")
    val dets = for (skipCol <- 0 until matrix.colCount) yield {
      val minor = matrix.minorMatrix(matrix.rowCount - 1, skipCol).det
      assert(minor.isInt, "Non-integer minor")
      minor.num
    }
    val gcd = dets.reduceLeft(_ gcd _)
    (gcd, dets)
  }
}
