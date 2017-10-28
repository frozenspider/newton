package org.newtonpolyhedron.math.internal

import org.newtonpolyhedron.math.MathImports._

import org.newtonpolyhedron.math.internal.FieldElementSupport._
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.FieldLUDecomposition
import spire.math.Numeric

class InternalMatrixMathProcessor(implicit mp: MathProcessor[Product])
    extends MatrixMathProcessor[Product] {
  private type N = Product
  private type M = Matrix[Product]
  private type AM = ApacheMatrix[Product]
  private val AM = ApacheMatrix

  //
  // Processing
  //

  override def add(m1: M, m2: M): M = AM.to(AM.from(m1) + AM.from(m2))

  override def subtract(m1: M, m2: M): M = AM.to(AM.from(m1) - AM.from(m2))

  override def multiply(m1: M, m2: M): M = AM.to(AM.from(m1) * AM.from(m2))

  override def negate(m: M): M = AM.to(-AM.from(m))

  override def inverse(m: M): M = {
    require(m.isSquare, "Non-square matrix")
    val am = AM.from(m)
    val amInv = new ApacheMatrix(new FieldLUDecomposition(am.underlying).getSolver.getInverse)(am.wrapper)
    AM.to(amInv)
  }

  override def minor(m: M, skipRow: Int, skipCol: Int): N =
    det(minorMatrix(m, skipRow, skipCol))

  override def minorMatrix(m: M, skipRow: Int, skipCol: Int): M = {
    val rows = ((0 until m.rowCount) filter (_ != skipRow)).toArray[Int]
    val cols = ((0 until m.colCount) filter (_ != skipCol)).toArray[Int]
    val am = AM.from(m)
    val amm = new ApacheMatrix(am.underlying.getSubMatrix(rows, cols))(am.wrapper)
    AM.to(amm)
  }

  override def det(m: M): N = {
    require(m.isSquare, "Non-square matrix")
    if (m.rowCount == 1) m(0, 0)
    else {
      // FieldLUDecomposition uses division, which is not acceptable for integer matrix
      // new FieldLUDecomposition[T](matrix).getDeterminant
      val additiveMinors = for (c <- 0 until m.colCount) yield m(0, c) * minor(m, 0, c)
      val last = if (additiveMinors.size % 2 != 0) additiveMinors.last else mp.zero
      additiveMinors.grouped(2) map { s =>
        if (s.size == 1) s(0) else s(0) - s(1)
      } reduce (_ + _)
    }
  }

  override def rank(m: M): Int = {
    require(m.rowCount != 0, "0-row matrix")
    require(m.colCount != 0, "0-column matrix")
    val zero = Product.zero
    // Zero matrix check
    val zeroMatrix = m.elementsByRow.forall(_._3 == zero)
    if (zeroMatrix) 0
    else {
      val triangle = triangleForm(m)._1
      val dim = math.min(m.rowCount, m.colCount)
      val diagonal = for (n <- 0 until dim) yield triangle(n, n)
      diagonal prefixLength (_ != zero)
    }
  }

  override def map[A, B](m: Matrix[A], f: A => B): Matrix[B] = {
    Matrix(m.rows.map(_.map(f)))
  }

  //
  // Advanced operations
  //

  override def triangleForm(m: M): (M, Int) = {
    val minDim = math.min(m.rowCount, m.colCount)
    val zero = Product.zero
    var (mutableCopy, wrapper) = {
      val am = AM.from(m)
      (am.underlying, am.wrapper)
    }

    def getSwapRow(row: Int, col: Int): Option[(N, Int)] = {
      val currElement = mutableCopy.getEntry(row, col).pure
      if (currElement != zero)
        Some((currElement, row))
      else if (row + 1 < m.rowCount)
        getSwapRow(row + 1, col)
      else None
    }

    def swapRows(i: Int, j: Int): Unit = {
      val row = mutableCopy.getRowVector(i)
      mutableCopy.setRowVector(i, mutableCopy.getRowVector(j))
      mutableCopy.setRowVector(j, row)
    }

    var sign = 1
    // currIdx tracks a corner element - row and column index
    for (currIdx <- 0 until minDim) {
      getSwapRow(currIdx, currIdx) match {
        case Some((cornerElement, swapRowIdx)) =>
          // Swap rows
          if (swapRowIdx != currIdx) {
            sign = -sign
            swapRows(currIdx, swapRowIdx)
          }

          val currRow = mutableCopy.getRowVector(currIdx)
          for (i <- (currIdx + 1) until m.rowCount) {
            val row = mutableCopy.getRowVector(i)
            val coeff = row.getEntry(currIdx) divide currRow.getEntry(currIdx)
            val otherRow = row subtract currRow.mapMultiply(coeff)
            mutableCopy.setRowVector(i, otherRow)
          }
        case None => // Curr column contains only zeros
        // Nothing to do here - just continue to next row
        // Note, that this means, that determinant is zero
      }
    }
    (AM.to(new ApacheMatrix(mutableCopy)(wrapper)), sign)
  }

  override def diagonalize(m: M): (M, M, M) = {
    val (a, b, c) = MatrixToDiagonalForm.toDiagonal(map(m, { (x: Product) => x.toRational }))
    val toProduct = (x: Rational) => Product(x)
    (map(a, toProduct), map(b, toProduct), map(c, toProduct))
  }
}
