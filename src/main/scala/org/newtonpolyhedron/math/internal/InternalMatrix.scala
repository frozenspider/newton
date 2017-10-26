package org.newtonpolyhedron.math.internal

import scala.collection.mutable.StringBuilder

import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.newtonpolyhedron.math.MathImports._
import org.newtonpolyhedron.math.internal.FieldElementSupport._

import spire.math.Numeric

class InternalMatrix[T](
  private val matrix: FieldMatrix[FieldElementWrapping[T]]
)(
  implicit
  protected[internal] val wrapper: FieldElementWrapper[T]
)
    extends MPMatrix
    with Function2[Int, Int, T]
    with Serializable {

  implicit protected[internal] def numeric: Numeric[T] = wrapper.numeric
  implicit def field: FieldWrapped[T] = wrapper.field

  protected[internal] def underlying = matrix

  def rowCount: Int = matrix.getRowDimension
  def colCount: Int = matrix.getColumnDimension

  def apply(row: Int, col: Int): T = this.matrix.getEntry(row, col).pure
  def +(that: InternalMatrix[T]) = new InternalMatrix(this.matrix add that.matrix)
  def -(that: InternalMatrix[T]) = new InternalMatrix(this.matrix subtract that.matrix)
  def *(that: InternalMatrix[T]) = new InternalMatrix(this.matrix multiply that.matrix)
  def unary_- = InternalMatrix.zero(rowCount, colCount)(wrapper.numeric) - this

  def transpose = new InternalMatrix(this.matrix.transpose)

  def contentCopy = matrix.copy

  // Generic `map` won't be available through MathProcessor
  def map[B: Numeric](f: T => B): InternalMatrix[B] = {
    implicit val wrapper2 = wrap[B]
    val mapped = new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper2.field, rowCount, colCount))
    for {
      r <- 0 until rowCount
      c <- 0 until colCount
    } mapped.underlying.setEntry(r, c, wrapper2(f(apply(r, c))))
    mapped
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: InternalMatrix[T] => this.matrix equals that.matrix
    case _                       => false
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

object InternalMatrix {

  def apply[T: Numeric](elements: Iterable[Iterable[T]]): InternalMatrix[T] = {
    require(!elements.isEmpty, "Elements was empty")
    implicit val wrapper = wrap[T]
    val dim = elements.head.size
    val apacheMath3Matrix = MatrixUtils.createFieldMatrix(wrapper.field, elements.size, dim)
    val elsSeq = elements.toIndexedSeq
    for {
      r <- 0 until elsSeq.size
      c <- 0 until dim
      vec = elsSeq(r).toIndexedSeq
      value = vec(c)
    } apacheMath3Matrix.setEntry(r, c, wrapper(value))
    new InternalMatrix(apacheMath3Matrix)
  }

  def idenitiy[T: Numeric](dim: Int): InternalMatrix[T] = {
    implicit val wrapper = wrap[T]
    new InternalMatrix(MatrixUtils.createFieldIdentityMatrix(wrapper.field, dim))
  }

  def zero[T: Numeric](dim: Int): InternalMatrix[T] = {
    implicit val wrapper = wrap[T]
    new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper.field, dim, dim))
  }

  def zero[T: Numeric](rowCount: Int, colCount: Int): InternalMatrix[T] = {
    implicit val wrapper = wrap[T]
    new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper.field, rowCount, colCount))
  }

  def empty[T: Numeric]: InternalMatrix[T] = {
    implicit val wrapper = wrap[T]
    new InternalMatrix(MatrixUtils.createFieldMatrix(wrapper.field, 0, 0))
  }
}
