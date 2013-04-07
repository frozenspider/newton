package org.newtonpolyhedron

import java.util.Comparator
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.BigIntFielded
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.Surface
import scala.collection.immutable.SortedSet

/**
 * Contains test shortcuts
 */
package object test {
  def matrInt(content: Array[Array[Int]]): Matrix[BigIntFielded] = {
    Matrix(content map (_ map (x => BigIntFielded(x))))
  }

  def matrFrac(content: Array[Array[Int]]): Matrix[BigFrac] = {
    Matrix(content map (_ map (x => BigFrac(x))))
  }

  def s[T](values: T*): IndexedSeq[T] = IndexedSeq(values: _*)

  def a(values: Int*): Array[Int] = Array(values: _*)
  def a(values: Array[Int]*): Array[Array[Int]] = Array(values: _*)
  def a(values: Array[Array[Int]]*): Array[Array[Array[Int]]] = Array(values: _*)

  def iv(ints: Int*): IntMathVec = IntMathVec(ints: _*)
  def fv(ints: Int*): FracMathVec = FracMathVec.fromInts(ints: _*)
  def fv2(fracs: BigFrac*): FracMathVec = FracMathVec(fracs: _*)

  def bf(n: Int, d: Int) = BigFrac(n, d)

  val intCmp = new Comparator[Int] { override def compare(i1: Int, i2: Int) = i1 compare i2 }

  def fillTableIdxKeys(lookupTable: KeyTable[IntMathVec, Int, Boolean],
                       upTo: Int): Unit = {
    for (i <- 0 until upTo)
      lookupTable.put(null, i, false)
    lookupTable.removeRow(null)
  }
  def markInTable(lookupTable: KeyTable[IntMathVec, Int, Boolean])(rowKey: IntMathVec)(toMark: Seq[Int]): Unit = {
    toMark map (lookupTable.put(rowKey, _, true))
    KeyTables.sortByColHeaders(lookupTable, intCmp, true)
  }
  /**
   * @param points sequence of [sequence of (pointIndices, upperLvlIndices)], one per dimension,
   * 				in order of descending dimensinon. Last sequence will get the dimension of 0
   */
  def chainSurfaces(points: Seq[IndexedSeq[(Seq[Int], Seq[Int])]]): Map[Int, SortedSet[Surface]] = {
    def addToMap(acc: Map[Int, SortedSet[Surface]],
                 dim: Int,
                 current: IndexedSeq[(Seq[Int], Seq[Int])],
                 remaining: Seq[IndexedSeq[(Seq[Int], Seq[Int])]]): Map[Int, SortedSet[Surface]] = {
      val upperLevel = (acc getOrElse (dim + 1, IndexedSeq())).toIndexedSeq
      val currentSurfaces = current map { s => new Surface(s._1, s._2 map upperLevel) }
      val newAcc = acc + (dim -> SortedSet(currentSurfaces: _*))
      assert(remaining.isEmpty ^ (dim != 0))
      if (remaining.isEmpty)
        newAcc
      else
        addToMap(newAcc, dim - 1, remaining.head, remaining.tail)
    }
    addToMap(Map(), points.size - 1, points.head, points.tail)
  }
}