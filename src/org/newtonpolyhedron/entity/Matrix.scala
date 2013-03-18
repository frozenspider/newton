package org.newtonpolyhedron.entity
import org.apache.commons.math3.Field
import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.linear.FieldLUDecomposition
import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.linear.MatrixUtils
import scala.collection.mutable.StringBuilder

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
    require(matrix.isSquare, "Non-square matrix")
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

  def apply[T <: FieldElement[T]](elements: Array[Array[T]]) =
    new Matrix(MatrixUtils.createFieldMatrix(elements))

  def idenitiy[T <: FieldElement[T]](dim: Int)(implicit field: Field[T]) =
    new Matrix(MatrixUtils.createFieldIdentityMatrix(field, dim))

  def zero[T <: FieldElement[T]](dim: Int)(implicit field: Field[T]) =
    new Matrix(MatrixUtils.createFieldMatrix(field, dim, dim))

  def zero[T <: FieldElement[T]](rowNum: Int, colNum: Int)(implicit field: Field[T]) =
    new Matrix(MatrixUtils.createFieldMatrix(field, rowNum, colNum))

}