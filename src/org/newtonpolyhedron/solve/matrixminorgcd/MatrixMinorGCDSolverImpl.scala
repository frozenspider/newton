package org.newtonpolyhedron.solve.matrixminorgcd
import java.math.BigInteger

import org.apache.commons.lang3.tuple.ImmutablePair
import org.apache.commons.lang3.tuple.Pair
import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix

class MatrixMinorGCDSolverImpl extends MatrixMinorGCDSolver {
  override def getLastRowGcd(matrix: FieldMatrix[BigFraction]): Pair[Integer, java.util.List[BigInteger]] = {
    val res = lastRowGcd(matrix)
    ImmutablePair.of(res._1.intValue, seq2list(res._2 map (_.underlying)))
  }

  override def lastRowGcd(matrix: Matrix[BigFrac]): (BigInt, Seq[BigInt]) = {
    require(matrix.isSquare, "Matrix must be square (although last row is zeros)")
    val dets = for (skipCol <- 0 until matrix.colNum) yield {
      val minor = matrix.minorMatrix(matrix.rowNum - 1, skipCol).det
      assert(minor.isInt, "Non-integer minor")
      minor.num
    }
    val gcd = dets.reduceLeft(_ gcd _)
    (gcd, dets)
  }
}