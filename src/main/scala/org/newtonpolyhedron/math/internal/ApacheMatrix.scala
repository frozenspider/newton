package org.newtonpolyhedron.math.internal

import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.linear.MatrixUtils
import org.newtonpolyhedron.math.MathImports._
import org.newtonpolyhedron.math.internal.FieldElementSupport._

import spire.math.Numeric

/**
 * Thin wrapper above Apache Math3's FieldMatrix
 *
 * @author FS
 */
class ApacheMatrix[T](
  protected[internal] val underlying: FieldMatrix[FieldElementWrapping[T]]
)(
  implicit
  protected[internal] val wrapper: FieldElementWrapper[T]
)
    extends Function2[Int, Int, T]
    with Serializable {

  implicit protected[internal] def numeric: Numeric[T] = wrapper.numeric
  implicit def field: FieldWrapped[T] = wrapper.field

  def rowCount: Int = underlying.getRowDimension
  def colCount: Int = underlying.getColumnDimension

  def apply(row: Int, col: Int): T = this.underlying.getEntry(row, col).pure
  def +(that: ApacheMatrix[T]) = new ApacheMatrix(this.underlying add that.underlying)
  def -(that: ApacheMatrix[T]) = new ApacheMatrix(this.underlying subtract that.underlying)
  def *(that: ApacheMatrix[T]) = new ApacheMatrix(this.underlying multiply that.underlying)
  def unary_- = ApacheMatrix.from(Matrix.zero(rowCount, colCount)) - this

  def transpose = new ApacheMatrix(this.underlying.transpose)

  def contentCopy = underlying.copy

  // Generic `map` won't be available through MathProcessor
  def map[B: Numeric](f: T => B): ApacheMatrix[B] = {
    implicit val wrapper2 = wrap[B]
    val mapped = new ApacheMatrix(MatrixUtils.createFieldMatrix(wrapper2.field, rowCount, colCount))
    for {
      r <- 0 until rowCount
      c <- 0 until colCount
    } mapped.underlying.setEntry(r, c, wrapper2(f(apply(r, c))))
    mapped
  }
}

object ApacheMatrix {
  def from[T: Numeric](m: Matrix[T]): ApacheMatrix[T] = {
    implicit val wrapper = wrap[T]
    require(!m.isEmpty, "Elements was empty")
    val apacheMath3Matrix = MatrixUtils.createFieldMatrix(wrapper.field, m.rowCount, m.colCount)
    m.elementsByRow.foreach {
      case (r, c, value) => apacheMath3Matrix.setEntry(r, c, wrapper(value))
    }
    new ApacheMatrix(apacheMath3Matrix)
  }

  def to[T: Numeric](m: ApacheMatrix[T]): Matrix[T] = {
    val content = for {
      r <- 0 until m.underlying.getRowDimension
    } yield for {
      c <- 0 until m.underlying.getColumnDimension
    } yield m.underlying.getEntry(r, c).pure
    Matrix(content)
  }
}
