package org.newtonpolyhedron

import java.util.Comparator
import scala.collection.immutable.SortedSet
import org.fs.utils.collection.table._
import org.newtonpolyhedron.entity._
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.scalatest.Suite
import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PolynomialUtils._

/**
 * Contains test shortcuts
 */
package object test {
  type DoubleConvertible = Any { def toDouble: Double }

  def matrInt(content: Seq[Seq[Int]]): Matrix[BigInt] = {
    Matrix(content map (_.toIndexedSeq map BigInt.apply))
  }

  def matrFrac(content: Seq[Seq[Int]]): Matrix[BigFrac] = {
    Matrix(content map (_.toIndexedSeq map BigFrac.apply))
  }

  def s[T](values: T*): IndexedSeq[T] = IndexedSeq(values: _*)

  def iv(ints: Int*): IntVec = IntVec((ints map BigInt.apply): _*)
  def fv(ints: Int*): FracVec = FracVec((ints map BigFrac.apply): _*)
  def fv2(fracs: BigFrac*): FracVec = FracVec(fracs: _*)

  def bf(n: Int) = BigFrac(n, 1)
  def bf(n: Int, d: Int) = BigFrac(n, d)

  def makePoly(components: (Int, Seq[Int])*): Polynomial =
    components map { case (coeff, pows) => Term(Product(coeff), fv(pows: _*)) } toIndexedSeq

  val intCmp = new Comparator[Int] { override def compare(i1: Int, i2: Int) = i1 compare i2 }

  def fillTableIdxKeys(lookupTable: KeyTable[IntVec, Int, Boolean],
                       upTo: Int): Unit = {
    for (i <- 0 until upTo)
      lookupTable.put(null, i, false)
    lookupTable.removeRow(null)
  }
  def markInTable(lookupTable: KeyTable[IntVec, Int, Boolean])(rowKey: IntVec)(toMark: Seq[Int]): Unit = {
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

  implicit def toDoubleApproximateEquals[T1 <: DoubleConvertible](thisDouble: T1) = new {
    val eps = 0.000001

    /** This approximately equals that */
    def =~=[T2 <: DoubleConvertible](that: T2): Option[String] = {
      if ((thisDouble.toDouble - that.toDouble).abs < eps) None
      else Some(s"$thisDouble is not close to $that")
    }
  }
}
