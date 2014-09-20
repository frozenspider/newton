package org.newtonpolyhedron.entity

import scala.collection.mutable.StringBuilder

import org.apache.commons.math3.Field
import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.linear.FieldLUDecomposition
import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.linear.FieldVector
import org.apache.commons.math3.linear.MatrixUtils
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.vector.VectorImports._

class Matrix[T <: FieldElement[T]](private val matrix: FieldMatrix[T])
    extends Function2[Int, Int, T]
    with Serializable {

  implicit protected[entity] val field = this.matrix.getField

  val rowCount = this.matrix.getRowDimension
  val colCount = this.matrix.getColumnDimension
  val isSquare = this.matrix.isSquare

  def apply(row: Int, col: Int) = this.matrix.getEntry(row, col)
  def +(that: Matrix[T]) = new Matrix(this.matrix add that.matrix)
  def -(that: Matrix[T]) = new Matrix(this.matrix subtract that.matrix)
  def *(that: Matrix[T]) = new Matrix(this.matrix multiply that.matrix)
  def unary_- = Matrix.zero(rowCount, colCount) - this

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
      val additiveMinors = for (c <- 0 until colCount) yield this(0, c) multiply minor(0, c)
      val last = if (additiveMinors.size % 2 != 0) additiveMinors.last else field.getZero
      additiveMinors.grouped(2) map { s =>
        if (s.size == 1) s(0)
        else s(0) subtract s(1)
      } reduce (_ add _)
    }
  }

  lazy val rank = {
    // TODO: Delegate to library
    require(rowCount != 0, "0-row matrix")
    require(colCount != 0, "0-column matrix")
    val zero = field.getZero
    // Zero matrix check
    val zeroMatrix = elementsByRow.forall(_._3 == zero)
    if (zeroMatrix) 0
    else {
      val triangle = this.triangleForm._1
      val dim = rowCount min colCount
      val diagonal = for (n <- 0 until dim) yield triangle(n, n)
      diagonal prefixLength (_ != zero)
    }
  }

  def map[B <: FieldElement[B]](f: T => B)(implicit field: Field[B]) = {
    val mapped = Matrix.zero[B](rowCount, colCount)
    for {
      r <- 0 until rowCount
      c <- 0 until colCount
    } mapped.matrix setEntry (r, c, f(this.matrix getEntry (r, c)))
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
    val minDim = rowCount min colCount
    val zero = field.getZero
    var mutableCopy = contentCopy

    def getSwapRow(row: Int, col: Int): Option[(T, Int)] = {
      val currElement = mutableCopy.getEntry(row, col)
      if (currElement != zero)
        Some((currElement, row))
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

  def addRow(row: Traversable[T]) = {
    require(row.size == colCount, "Wrong row size")
    val res = Matrix.zero[T](rowCount + 1, colCount)
    res.matrix.setSubMatrix(this.matrix.getData, 0, 0)
    val rowSeq = row.toIndexedSeq
    for (i <- 0 until colCount) res.matrix setEntry (rowCount, i, rowSeq(i))
    res
  }

  def addCol(col: Traversable[T]) = {
    require(col.size == rowCount, "Wrong col size")
    val res = Matrix.zero[T](rowCount, colCount + 1)
    res.matrix.setSubMatrix(this.matrix.getData, 0, 0)
    val colSeq = col.toIndexedSeq
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
      colsWidth(col) = colsWidth(col) max this(row, col).toString.length
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

  def fromArray[T <: FieldElement[T]](elements: Array[Array[T]]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(elements))

  def fromIntVectors(elements: Iterable[IntVec]): Matrix[BigIntFielded] = {
    fromArray((elements map (x => (x map int2Fielded).toArray)).toArray)
  }

  def fromVectors[T <: FieldElement[T]](elements: Iterable[IndexedSeq[T]])(implicit field: Field[T]): Matrix[T] = {
    require(!elements.isEmpty, "Elements was empty")
    val dim = elements.head.size
    val matrix = MatrixUtils.createFieldMatrix(field, elements.size, dim)
    val elsSeq = elements.toIndexedSeq
    for {
      r <- 0 until elsSeq.size
      c <- 0 until dim
      val vec = elsSeq(r)
      val value = vec(c)
    } matrix.setEntry(r, c, value)
    new Matrix(matrix)
  }

  def idenitiy[T <: FieldElement[T]](dim: Int)(implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldIdentityMatrix(field, dim))

  def zero[T <: FieldElement[T]](dim: Int)(implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(field, dim, dim))

  def zero[T <: FieldElement[T]](rowCount: Int, colCount: Int)(implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(field, rowCount, colCount))

  def empty[T <: FieldElement[T]](implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(field, 0, 0))

  /**
   * Converts the matrix to diagonal form.
   * <p>
   * Returns diagonal matrix alongside with row and column transformation matrices
   */
  def toDiagonal(m: Matrix[BigFrac]): (Matrix[BigFrac], Matrix[BigFrac], Matrix[BigFrac]) = {
    require(m.isSquare, "Non-square matrix")
    import MatrixExt._
    m.toDiagonal
    //    val iden = idenitiy(m.rowCount)(m.field)
    //    val rowOnes = iden.contentCopy
    //    val colOnes = iden.contentCopy
    //    toDiagonalFormInternal(m.contentCopy, 0, rowOnes, colOnes, m.rowCount)
  }

  private def toDiagonalFormInternal(m: FieldMatrix[BigFrac],
                                     cornerIdx: Int,
                                     rowOnes: FieldMatrix[BigFrac],
                                     colOnes: FieldMatrix[BigFrac],
                                     dim: Int)(implicit field: Field[BigFrac]): (Matrix[BigFrac], Matrix[BigFrac], Matrix[BigFrac]) = {
    def processCorner(getCurrent: (Int, Int) => BigFrac,
                      ones: FieldMatrix[BigFrac],
                      swap: (FieldMatrix[BigFrac], Int, Int) => Unit,
                      inverse: (FieldMatrix[BigFrac], Int) => Unit,
                      subtractMultiplied: (FieldMatrix[BigFrac], Int, Int, BigFrac) => Unit)(currIdx: Int,
                                                                                             corner: BigFrac,
                                                                                             atLeastOneFails: Boolean): (BigFrac, Boolean) = {
      def recurse(i: Int, corner: BigFrac, fail: Boolean): (BigFrac, Boolean) = {
        if (i == dim) (corner, fail)
        else {
          val current = getCurrent(i, cornerIdx)
          if (corner == 0) {
            if (current != 0) {
              swap(m, cornerIdx, i)
              inverse(m, i)

              swap(ones, cornerIdx, i)
              inverse(ones, i)

              recurse(i + 1, m.getEntry(cornerIdx, cornerIdx), true)
            } else {
              recurse(i + 1, corner, fail)
            }
          } else {
            val div = current / corner
            if (div.remainder != 0) {
              subtractMultiplied(m, cornerIdx, i, BigFrac(div.quotient))
              swap(m, cornerIdx, i)
              inverse(m, i)

              subtractMultiplied(ones, cornerIdx, i, BigFrac(div.quotient))
              swap(ones, cornerIdx, i)
              inverse(ones, i)

              recurse(i, m.getEntry(cornerIdx, cornerIdx), true)
            } else {
              recurse(i + 1, corner, fail)
            }
          }
        }
      }
      recurse(currIdx, corner, atLeastOneFails)
    }
    def getCurrCol = (curr: Int, corner: Int) => m.getEntry(corner, curr)
    def getCurrRow = (curr: Int, corner: Int) => m.getEntry(curr, corner)
    def processCornerCol = processCorner(getCurrCol, colOnes, swapCols, inverseCol, subtractMultipliedCol)_
    def processCornerRow = processCorner(getCurrRow, rowOnes, swapRows, inverseRow, subtractMultipliedRow)_
    def processCornerFrom(corner: BigFrac): BigFrac = {
      val (corner1, failCol) = processCornerCol(cornerIdx + 1, corner, false)
      val (corner2, failRow) = processCornerRow(cornerIdx + 1, corner1, false)
      if (!failRow && !failCol) corner2
      else processCornerFrom(corner2)
    }
    // Make corner element a GCD of it's row and column
    val corner = processCornerFrom(m.getEntry(cornerIdx, cornerIdx))

    // Subtract multiplied first row/column from others rows/columns
    // so that corner element remains only non-zero element in it's row/column
    for (colIdx <- (cornerIdx + 1) until dim) {
      val current = m.getEntry(cornerIdx, colIdx)
      val quotient = (current / corner).quotient
      if (quotient != 0) {
        subtractMultipliedCol(m, cornerIdx, colIdx, BigFrac(quotient))
        subtractMultipliedCol(colOnes, cornerIdx, colIdx, BigFrac(quotient))
      }
    }
    for (rowIdx <- (cornerIdx + 1) until dim) {
      val current = m.getEntry(rowIdx, cornerIdx)
      val quotient = (current / corner).quotient
      if (quotient != 0) {
        subtractMultipliedRow(m, cornerIdx, rowIdx, BigFrac(quotient))
        subtractMultipliedRow(rowOnes, cornerIdx, rowIdx, BigFrac(quotient))
      }
    }

    if (cornerIdx < dim - 1) {
      toDiagonalFormInternal(m, cornerIdx + 1, rowOnes, colOnes, dim)
    } else {
      (new Matrix(m), new Matrix(rowOnes), new Matrix(colOnes))
    }
  }

  /** {@code dst = dst - src*quot} */
  private def subtractMultipliedCol[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                                          srcColIdx: Int,
                                                          dstColIdx: Int,
                                                          quotient: T): Unit = {
    val srcCol = matrix.getColumnVector(srcColIdx)
    val dstCol = matrix.getColumnVector(dstColIdx)
    val srcColMul = srcCol mapMultiply quotient
    val dstColSub = dstCol subtract srcColMul
    matrix.setColumnVector(dstColIdx, dstColSub)
  }

  /** {@code dst = dst - src*quot} */
  private def subtractMultipliedRow[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                                          srcRowIdx: Int,
                                                          dstRowIdx: Int,
                                                          quotient: T): Unit = {
    val srcRow = matrix.getRowVector(srcRowIdx)
    val dstRow = matrix.getRowVector(dstRowIdx)
    val srcRowMul = srcRow mapMultiply quotient
    val dstRowSub = dstRow subtract srcRowMul
    matrix.setRowVector(dstRowIdx, dstRowSub)
  }

  private def swapCols[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                             idx1: Int,
                                             idx2: Int) = {
    val col1 = matrix.getColumnVector(idx1)
    val col2 = matrix.getColumnVector(idx2)
    matrix.setColumnVector(idx1, col2)
    matrix.setColumnVector(idx2, col1)
  }

  private def swapRows[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                             idx1: Int,
                                             idx2: Int) = {
    val row1 = matrix.getRowVector(idx1)
    val row2 = matrix.getRowVector(idx2)
    matrix.setRowVector(idx1, row2)
    matrix.setRowVector(idx2, row1)
  }

  private def fieldRowOfZeros[T <: FieldElement[T]](size: Int)(implicit field: Field[T]): FieldVector[T] =
    MatrixUtils.createFieldMatrix(field, size, 1).getColumnVector(0)

  private def inverseCol[T <: FieldElement[T]](m: FieldMatrix[T], idx: Int)(implicit field: Field[T]): Unit = {
    val vec = m.getColumnVector(idx)
    val zeros = fieldRowOfZeros(vec.getDimension)
    m.setColumnVector(idx, zeros subtract vec)
  }

  private def inverseRow[T <: FieldElement[T]](m: FieldMatrix[T], idx: Int)(implicit field: Field[T]): Unit = {
    val vec = m.getRowVector(idx)
    val zeros = fieldRowOfZeros(vec.getDimension)
    m.setRowVector(idx, zeros subtract vec)
  }
}