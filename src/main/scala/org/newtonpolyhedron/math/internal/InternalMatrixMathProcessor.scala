package org.newtonpolyhedron.math.internal

import org.newtonpolyhedron.math.MathImports._

import org.newtonpolyhedron.math.internal.FieldElementSupport._
import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.linear.FieldLUDecomposition
import spire.math.Numeric

class InternalMatrixMathProcessor(implicit mp: MathProcessor[Product, InternalMatrix[Product]])
    extends MatrixMathProcessor[Product, InternalMatrix[Product]] {
  private type N = Product
  private type M = InternalMatrix[Product]

  //
  // Construction
  //

  override def apply(elements: Iterable[Iterable[N]]): M = {
    require(!elements.isEmpty, "Elements was empty")
    implicit val wrapper = wrap[N]
    val dim = elements.head.size
    val apacheMath3Matrix = MatrixUtils.createFieldMatrix(wrapper.field, elements.size, dim)
    val elsSeq = elements.toIndexedSeq
    for {
      r <- 0 until elsSeq.size
      c <- 0 until dim
      val vec = elsSeq(r).toIndexedSeq
      val value = vec(c)
    } apacheMath3Matrix.setEntry(r, c, wrapper(value))
    new InternalMatrix(apacheMath3Matrix)
  }

  override def idenitiy(dim: Int): M = {
    implicit val wrapper = wrap[N]
    new InternalMatrix(MatrixUtils.createFieldIdentityMatrix(wrapper.field, dim))
  }

  override def zero(dim: Int): M = {
    implicit val wrapper = wrap[N]
    new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper.field, dim, dim))
  }

  override def zero(rowCount: Int, colCount: Int): M = {
    implicit val wrapper = wrap[N]
    new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper.field, rowCount, colCount))
  }

  override def empty: M = {
    implicit val wrapper = wrap[N]
    new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper.field, 0, 0))
  }

  //
  // Processing
  //

  override def get(m: M, row: Int, col: Int): N = m(row, col)

  override def add(m1: M, m2: M): M = m1 + m2

  override def subtract(m1: M, m2: M): M = m1 - m2

  override def multiply(m1: M, m2: M): M = m1 * m2

  override def negate(m: M): M = -m

  override def inverse(m: M): M = {
    require(m.isSquare, "Non-square matrix")
    new InternalMatrix(new FieldLUDecomposition(m.underlying).getSolver.getInverse)(m.wrapper)
  }

  override def transpose(m: M): M = m.transpose

  override def minor(m: M, skipRow: Int, skipCol: Int): N =
    minorMatrix(m, skipRow, skipCol).det

  override def minorMatrix(m: M, skipRow: Int, skipCol: Int): M = {
    val rows = ((0 until m.rowCount) filter (_ != skipRow)).toArray[Int]
    val cols = ((0 until m.colCount) filter (_ != skipCol)).toArray[Int]
    new InternalMatrix(m.underlying.getSubMatrix(rows, cols))(m.wrapper)
  }

  override def det(m: M): N = {
    require(m.isSquare, "Non-square matrix")
    if (m.rowCount == 1) m(0, 0)
    else {
      // FieldLUDecomposition uses division, which is not acceptable for integer matrix
      // new FieldLUDecomposition[T](matrix).getDeterminant
      val additiveMinors = for (c <- 0 until m.colCount) yield m(0, c) * minor(m, 0, c)
      val last = if (additiveMinors.size % 2 != 0) additiveMinors.last else m.field.getZero
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

  override def map(m: M, f: N => N): M =
    m map f

  override def elementsByRow(m: M): Stream[(Int, Int, N)] = {
    def elementsStrartingFrom(row: Int, col: Int): Stream[(Int, Int, N)] = {
      if (row >= m.rowCount || (row == m.rowCount - 1 && col >= m.colCount)) Stream.empty
      else if (col < m.colCount) (row, col, m(row, col)) #:: elementsStrartingFrom(row, col + 1)
      else elementsStrartingFrom(row + 1, 0)
    }
    elementsStrartingFrom(0, 0)
  }

  override def rows(m: M): IndexedSeq[IndexedSeq[N]] = {
    (0 until m.rowCount) map (i => m.underlying.getRow(i).toIndexedSeq map (_.pure))
  }

  override def cols(m: M): IndexedSeq[IndexedSeq[N]] = {
    (0 until m.colCount) map (i => m.underlying.getColumn(i).toIndexedSeq map (_.pure))
  }

  override def addRow(m: M, row: Traversable[N]): M = {
    require(row.size == m.colCount, "Wrong row size")
    val res = zero(m.rowCount + 1, m.colCount)
    res.underlying.setSubMatrix(m.underlying.getData, 0, 0)
    val rowSeq = row.toIndexedSeq map m.wrapper.apply
    for (i <- 0 until m.colCount) res.underlying setEntry (m.rowCount, i, rowSeq(i))
    res
  }

  override def addCol(m: M, col: Traversable[N]): M = {
    require(col.size == m.rowCount, "Wrong col size")
    val res = zero(m.rowCount, m.colCount + 1)
    res.underlying.setSubMatrix(m.underlying.getData, 0, 0)
    val colSeq = col.toIndexedSeq map m.wrapper.apply
    for (i <- 0 until m.rowCount) res.underlying setEntry (i, m.colCount, colSeq(i))
    res
  }

  //
  // Advanced operations
  //

  override def triangleForm(m: M): (M, Int) = {
    val minDim = math.min(m.rowCount, m.colCount)
    val zero = Product.zero
    var mutableCopy = m.contentCopy

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
    (new InternalMatrix(mutableCopy)(m.wrapper), sign)
  }

  override def diagonalize(m: M): (M, M, M) = {
    val (a, b, c) = MatrixToDiagonalForm.toDiagonal(m map (_.toRational))
    ((a map Product.apply), (b map Product.apply), (c map Product.apply))
  }
}
