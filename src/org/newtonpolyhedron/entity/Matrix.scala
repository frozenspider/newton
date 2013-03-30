package org.newtonpolyhedron.entity
import org.apache.commons.math3.Field
import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.linear.FieldLUDecomposition
import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.linear.MatrixUtils
import scala.collection.mutable.StringBuilder
import org.apache.commons.math3.linear.FieldVector
import org.apache.commons.math3.linear.SingularValueDecomposition
import scala.collection.mutable.ArraySeq

class Matrix[T <: FieldElement[T]](private val matrix: FieldMatrix[T])
    extends Function2[Int, Int, T]
    with Serializable {

  implicit private lazy val field = this.matrix.getField

  val rowNum = this.matrix.getRowDimension
  val colNum = this.matrix.getColumnDimension
  val isSquare = this.matrix.isSquare

  def apply(row: Int, col: Int) = this.matrix.getEntry(row, col)
  def +(that: Matrix[T]) = new Matrix(this.matrix add that.matrix)
  def -(that: Matrix[T]) = new Matrix(this.matrix subtract that.matrix)
  def *(that: Matrix[T]) = new Matrix(this.matrix multiply that.matrix)
  def unary_- = Matrix.zero(rowNum, colNum) - this

  /** Inverse */
  def inv = {
    require(isSquare, "Non-square matrix")
    new Matrix(new FieldLUDecomposition(matrix).getSolver.getInverse)
  }
  def transpose = new Matrix(this.matrix.transpose)

  lazy val det = {
    require(isSquare, "Non-square matrix")
    new FieldLUDecomposition[T](matrix).getDeterminant
  }
  lazy val rank = {
    // TODO: Delegate to library
    require(rowNum != 0, "0-row matrix")
    require(colNum != 0, "0-column matrix")
    val zero = field.getZero
    // Zero matrix check
    val zeroMatrix = elementsByRow.forall(_._3 == zero)
    if (zeroMatrix) 0
    else {
      val triangle = this.triangleForm._1
      val dim = rowNum min colNum
      val diagonal = for (n <- 0 until dim) yield triangle(n, n)
      diagonal prefixLength (_ != zero)
    }
  }

  def map[B <: FieldElement[B]](f: T => B)(implicit field: Field[B]) = {
    val mapped = Matrix.zero[B](rowNum, colNum)
    for {
      r <- 0 until rowNum
      c <- 0 until colNum
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
    val minDim = rowNum min colNum
    val zero = field.getZero
    var mutableCopy = contentCopy

    def getSwapRow(row: Int, col: Int): Option[(T, Int)] = {
      val currElement = mutableCopy.getEntry(row, col)
      if (currElement != zero)
        Some((currElement, row))
      else if (row + 1 < rowNum)
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
          for (i <- (currIdx + 1) until rowNum) {
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
      if (row >= rowNum || (row == rowNum - 1 && col >= colNum)) Stream.empty
      else if (col < colNum) (row, col, this(row, col)) #:: elementsStrartingFrom(row, col + 1)
      else elementsStrartingFrom(row + 1, 0)
    }
    elementsStrartingFrom(0, 0)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: Matrix[T] => this.matrix equals that.matrix
    case _               => false
  }
  override def hashCode = this.matrix.hashCode
  override def toString = {
    val colsWidth = Array.ofDim[Int](colNum)
    for {
      row <- 0 until rowNum
      col <- 0 until colNum
    } {
      colsWidth(col) = colsWidth(col) max this(row, col).toString.length
    }
    val result = new StringBuilder
    for (row <- 0 until rowNum) {
      for (col <- 0 until colNum) {
        result ++= String.format("%1$#" + colsWidth(col) + "s ", this(row, col).toString)
      }
      result += '\n'
    }
    result.deleteCharAt(result.length - 1).toString
  }
}

object Matrix {

  def apply[T <: FieldElement[T]](elements: Array[Array[T]]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(elements))

  def idenitiy[T <: FieldElement[T]](dim: Int)(implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldIdentityMatrix(field, dim))

  def zero[T <: FieldElement[T]](dim: Int)(implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(field, dim, dim))

  def zero[T <: FieldElement[T]](rowNum: Int, colNum: Int)(implicit field: Field[T]): Matrix[T] =
    new Matrix(MatrixUtils.createFieldMatrix(field, rowNum, colNum))

  /**
   * Converts the matrix to diagonal form.
   * <p>
   * Returns diagonal matrix alongside with row and column transformation matrices
   */
  def toDiagonal(m: Matrix[BigFrac]): (Matrix[BigFrac], Matrix[BigFrac], Matrix[BigFrac]) = {
    require(m.isSquare, "Non-square matrix")
    val iden = idenitiy(m.rowNum)(m.field)
    val rowOnes = iden.contentCopy
    val colOnes = iden.contentCopy
    toDiagonalFormInternal(m.contentCopy, 0, rowOnes, colOnes, m.rowNum)
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
            val div = current.divide(corner)
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
  def subtractMultipliedCol[T <: FieldElement[T]](matrix: FieldMatrix[T],
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
  def subtractMultipliedRow[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                                  srcRowIdx: Int,
                                                  dstRowIdx: Int,
                                                  quotient: T): Unit = {
    val srcRow = matrix.getRowVector(srcRowIdx)
    val dstRow = matrix.getRowVector(dstRowIdx)
    val srcRowMul = srcRow mapMultiply quotient
    val dstRowSub = dstRow subtract srcRowMul
    matrix.setRowVector(dstRowIdx, dstRowSub)
  }

  def swapCols[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                     idx1: Int,
                                     idx2: Int) = {
    val col1 = matrix.getColumnVector(idx1)
    val col2 = matrix.getColumnVector(idx2)
    matrix.setColumnVector(idx1, col2)
    matrix.setColumnVector(idx2, col1)
  }

  def swapRows[T <: FieldElement[T]](matrix: FieldMatrix[T],
                                     idx1: Int,
                                     idx2: Int) = {
    val row1 = matrix.getRowVector(idx1)
    val row2 = matrix.getRowVector(idx2)
    matrix.setRowVector(idx1, row2)
    matrix.setRowVector(idx2, row1)
  }

  private def fieldRowOfZeros[T <: FieldElement[T]](size: Int)(implicit field: Field[T]): FieldVector[T] =
    MatrixUtils.createFieldMatrix(field, size, 1).getColumnVector(0)

  def inverseCol[T <: FieldElement[T]](m: FieldMatrix[T], idx: Int)(implicit field: Field[T]): Unit = {
    val vec = m.getColumnVector(idx)
    val zeros = fieldRowOfZeros(vec.getDimension)
    m.setColumnVector(idx, zeros subtract vec)
  }

  def inverseRow[T <: FieldElement[T]](m: FieldMatrix[T], idx: Int)(implicit field: Field[T]): Unit = {
    val vec = m.getRowVector(idx)
    val zeros = fieldRowOfZeros(vec.getDimension)
    m.setRowVector(idx, zeros subtract vec)
  }
}