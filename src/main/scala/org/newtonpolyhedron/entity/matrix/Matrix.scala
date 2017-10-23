package org.newtonpolyhedron.entity.matrix

import scala.collection.mutable.StringBuilder

import org.apache.commons.math3.linear.FieldLUDecomposition
import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.linear.MatrixUtils

import org.newtonpolyhedron.entity.matrix.internal.FieldElementSupport._
import spire.implicits._
import spire.math.Numeric

class Matrix[T](private val matrix: FieldMatrix[FieldElementWrapping[T]])(implicit wrapper: FieldElementWrapper[T])
    extends Function2[Int, Int, T]
    with Serializable {

  implicit protected[entity] def numeric: Numeric[T] = wrapper.numeric
  implicit def field: FieldWrapped[T] = wrapper.field

  val rowCount: Int = this.matrix.getRowDimension
  val colCount: Int = this.matrix.getColumnDimension
  val isSquare: Boolean = this.matrix.isSquare

  def apply(row: Int, col: Int): T = this.matrix.getEntry(row, col).pure
  def +(that: Matrix[T]) = new Matrix(this.matrix add that.matrix)
  def -(that: Matrix[T]) = new Matrix(this.matrix subtract that.matrix)
  def *(that: Matrix[T]) = new Matrix(this.matrix multiply that.matrix)
  def unary_- = Matrix.zero(rowCount, colCount)(wrapper.numeric) - this

  /** Inverse */
  def inv = {
    require(isSquare, "Non-square matrix")
    new Matrix(new FieldLUDecomposition(matrix).getSolver.getInverse)
  }

  def transpose = new Matrix(this.matrix.transpose)

  def minor(skipRow: Int, skipCol: Int) =
    minorMatrix(skipRow, skipCol).det

  def minorMatrix(skipRow: Int, skipCol: Int) = {
    val rows = ((0 until rowCount) filter (_ != skipRow)).toArray[Int]
    val cols = ((0 until colCount) filter (_ != skipCol)).toArray[Int]
    new Matrix(matrix.getSubMatrix(rows, cols))
  }

  lazy val det: T = {
    require(isSquare, "Non-square matrix")
    if (rowCount == 1) this(0, 0)
    else {
      // FieldLUDecomposition uses division, which is not acceptable for integer matrix
      // new FieldLUDecomposition[T](matrix).getDeterminant
      val additiveMinors = for (c <- 0 until colCount) yield this(0, c) * minor(0, c)
      val last = if (additiveMinors.size % 2 != 0) additiveMinors.last else field.getZero
      additiveMinors.grouped(2) map { s =>
        if (s.size == 1) s(0) else s(0) - s(1)
      } reduce (_ + _)
    }
  }

  lazy val rank = {
    // TODO: Delegate to library
    require(rowCount != 0, "0-row matrix")
    require(colCount != 0, "0-column matrix")
    val zero = field.getZero.pure
    // Zero matrix check
    val zeroMatrix = elementsByRow.forall(_._3 == zero)
    if (zeroMatrix) 0
    else {
      val triangle = this.triangleForm._1
      val dim = math.min(rowCount, colCount)
      val diagonal = for (n <- 0 until dim) yield triangle(n, n)
      diagonal prefixLength (_ != zero)
    }
  }

  def map[B: Numeric](f: T => B) = {
    implicit val wrapper2 = wrap[B]
    val mapped = Matrix.zero[B](rowCount, colCount)(wrapper2.numeric)
    for {
      r <- 0 until rowCount
      c <- 0 until colCount
    } mapped.matrix setEntry (r, c, wrapper2(f(this.matrix.getEntry(r, c).pure)))
    mapped
  }

  /**
   * Converges the matrix to a triangle form, where all elements below main diagonal are zeros.
   * <p>
   * This operation doesn't change determinant value, but it may change it's sign.
   * <p>
   * Example of a triangle matrix (empty cells = zeros):
   *
   * <pre>
   * +--+--+--+--+
   * | 1| 2| 3| 4|
   * +--+--+--+--+
   * |  | 2| 3| 4|
   * +--+--+--+--+
   * |  |  | 3| 4|
   * +--+--+--+--+
   * |  |  |  | 4|
   * +--+--+--+--+
   * </pre>
   *
   * @return matrix and {@code 1} or {@code -1} depending on whether or not determinant sign was reversed
   */
  def triangleForm: (Matrix[T], Int) = {
    // TODO: Delegate to library
    val minDim = math.min(rowCount, colCount)
    val zero = field.getZero
    var mutableCopy = contentCopy

    def getSwapRow(row: Int, col: Int): Option[(T, Int)] = {
      val currElement = mutableCopy.getEntry(row, col)
      if (currElement != zero)
        Some((currElement.pure, row))
      else if (row + 1 < rowCount)
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
          for (i <- (currIdx + 1) until rowCount) {
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
    (new Matrix(mutableCopy), sign)
  }

  def contentCopy = matrix.copy

  def exists(cond: T => Boolean): Boolean = elementsByRow exists (e => cond(e._3))

  def forall(cond: T => Boolean): Boolean = elementsByRow forall (e => cond(e._3))

  def contains(what: T): Boolean = elementsByRow exists (_._3 == what)

  /** @return stream of (row, col, element) */
  def elementsByRow: Stream[(Int, Int, T)] = {
    def elementsStrartingFrom(row: Int, col: Int): Stream[(Int, Int, T)] = {
      if (row >= rowCount || (row == rowCount - 1 && col >= colCount)) Stream.empty
      else if (col < colCount) (row, col, this(row, col)) #:: elementsStrartingFrom(row, col + 1)
      else elementsStrartingFrom(row + 1, 0)
    }
    elementsStrartingFrom(0, 0)
  }

  def rows: IndexedSeq[IndexedSeq[T]] = {
    (0 until rowCount) map (i => matrix.getRow(i).toIndexedSeq map (_.pure))
  }

  def cols: IndexedSeq[IndexedSeq[T]] = {
    (0 until colCount) map (i => matrix.getColumn(i).toIndexedSeq map (_.pure))
  }

  def addRow(row: Traversable[T]) = {
    require(row.size == colCount, "Wrong row size")
    val res = Matrix.zero[T](rowCount + 1, colCount)(wrapper.numeric)
    res.matrix.setSubMatrix(this.matrix.getData, 0, 0)
    val rowSeq = row.toIndexedSeq map wrapper.apply
    for (i <- 0 until colCount) res.matrix setEntry (rowCount, i, rowSeq(i))
    res
  }

  def addCol(col: Traversable[T]) = {
    require(col.size == rowCount, "Wrong col size")
    val res = Matrix.zero[T](rowCount, colCount + 1)(wrapper.numeric)
    res.matrix.setSubMatrix(this.matrix.getData, 0, 0)
    val colSeq = col.toIndexedSeq map wrapper.apply
    for (i <- 0 until rowCount) res.matrix setEntry (i, colCount, colSeq(i))
    res
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Matrix[T] => this.matrix equals that.matrix
    case _               => false
  }
  override def hashCode = this.matrix.hashCode
  override def toString = {
    val colsWidth = Array.ofDim[Int](colCount)
    for {
      row <- 0 until rowCount
      col <- 0 until colCount
    } {
      colsWidth(col) = math.max(colsWidth(col), this(row, col).toString.length)
    }
    val result = new StringBuilder
    for (row <- 0 until rowCount) {
      for (col <- 0 until colCount) {
        result ++= String.format("%1$" + colsWidth(col) + "s ", this(row, col).toString)
      }
      result += '\n'
    }
    result.deleteCharAt(result.length - 1).toString
  }
}

object Matrix {

  def apply[T: Numeric](elements: Iterable[Iterable[T]]): Matrix[T] = {
    require(!elements.isEmpty, "Elements was empty")
    implicit val wrapper = wrap[T]
    val dim = elements.head.size
    val apacheMath3Matrix = MatrixUtils.createFieldMatrix(wrapper.field, elements.size, dim)
    val elsSeq = elements.toIndexedSeq
    for {
      r <- 0 until elsSeq.size
      c <- 0 until dim
      val vec = elsSeq(r).toIndexedSeq
      val value = vec(c)
    } apacheMath3Matrix.setEntry(r, c, wrapper(value))
    new Matrix(apacheMath3Matrix)
  }

  def idenitiy[T: Numeric](dim: Int): Matrix[T] = {
    implicit val wrapper = wrap[T]
    new Matrix(MatrixUtils.createFieldIdentityMatrix(wrapper.field, dim))
  }

  def zero[T: Numeric](dim: Int): Matrix[T] = {
    implicit val wrapper = wrap[T]
    new Matrix(MatrixUtils.createFieldMatrix(wrapper.field, dim, dim))
  }

  def zero[T: Numeric](rowCount: Int, colCount: Int): Matrix[T] = {
    implicit val wrapper = wrap[T]
    new Matrix(MatrixUtils.createFieldMatrix(wrapper.field, rowCount, colCount))
  }

  def empty[T: Numeric]: Matrix[T] = {
    implicit val wrapper = wrap[T]
    new Matrix(MatrixUtils.createFieldMatrix(wrapper.field, 0, 0))
  }
}
