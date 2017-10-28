package org.newtonpolyhedron.entity.math

import spire.math.Numeric
import algebra.ring.AdditiveMonoid

/**
 * Trait for internal matrix representation user by associated `MathProcessor`
 *
 * @author FS
 */
class Matrix[A] private (val rows: IndexedSeq[IndexedSeq[A]])
    extends ((Int, Int) => A) {

  lazy val rowCount: Int = rows.size
  lazy val colCount: Int = if (rows.size > 0) rows.head.size else 0
  def isSquare: Boolean = rowCount == colCount
  def isEmpty: Boolean = rowCount == 0

  def apply(row: Int, col: Int): A = {
    require(row >= 0 && row < rowCount, "Illegal row number")
    require(col >= 0 && col < colCount, "Illegal row number")
    rows(row)(col)
  }

  /** @return stream of (row, col, element) */
  def elementsByRow: Stream[(Int, Int, A)] = {
    def elementsStrartingFrom(row: Int, col: Int): Stream[(Int, Int, A)] = {
      if (row >= rowCount || (row == rowCount - 1 && col >= colCount)) Stream.empty
      else if (col < colCount) (row, col, this(row, col)) #:: elementsStrartingFrom(row, col + 1)
      else elementsStrartingFrom(row + 1, 0)
    }
    elementsStrartingFrom(0, 0)
  }

  def cols: IndexedSeq[IndexedSeq[A]] = {
    rows.transpose
  }

  def addRow(row: Traversable[A]): Matrix[A] = {
    val rowSeq = row.toIndexedSeq
    require(rowSeq.size == colCount, "Illegal row size")
    new Matrix(rows :+ rowSeq)
  }

  def addCol(col: Traversable[A]): Matrix[A] = {
    val colSeq = col.toIndexedSeq
    require(colSeq.size == rowCount, "Illegal row size")
    new Matrix((rows zip colSeq) map { case (row, el) => row :+ el })
  }

  def transpose: Matrix[A] = {
    new Matrix(cols)
  }

  //
  // Default methods
  //

  override def equals(obj: Any): Boolean = obj match {
    case that: Matrix[_] => this.rows equals that.rows
    case _               => false
  }

  override def hashCode = this.rows.hashCode

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
  def apply[A](rows: Iterable[Iterable[A]]): Matrix[A] = {
    val rowSizes = rows.map(_.size).toSeq.distinct
    require(rowSizes.size <= 1, "Row sizes should be equal")
    new Matrix(rows.map(_.toIndexedSeq).toIndexedSeq)
  }

  def idenitiy[A](dim: Int)(implicit supp: Numeric[A]): Matrix[A] = {
    val rows = (0 until dim) map (IndexedSeq.fill(dim)(supp.zero).updated(_, supp.one))
    new Matrix(rows)
  }

  def zero[A](dim: Int)(implicit supp: AdditiveMonoid[A]): Matrix[A] = {
    zero(dim, dim)
  }

  def zero[A](rowCount: Int, colCount: Int)(implicit supp: AdditiveMonoid[A]): Matrix[A] = {
    new Matrix(IndexedSeq.fill(rowCount, colCount)(supp.zero))
  }

  def empty[A]: Matrix[A] = {
    new Matrix(IndexedSeq.empty)
  }
}
