package org.newtonpolyhedron

import java.util.Comparator

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table._
import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.matrix.Matrix

/**
 * Contains test shortcuts
 */
package object test {
  type DoubleConvertible = Any { def toDouble: Double }

  def matrInt(content: Seq[Seq[Int]]): Matrix[BigInt] = {
    Matrix(content map (_.toIndexedSeq map BigInt.apply))
  }

  def matrFrac(content: Seq[Seq[Int]]): Matrix[Rational] = {
    Matrix(content map (_.toIndexedSeq map Rational.apply))
  }

  def matrNum[N <: MPNumber](content: Seq[Seq[Int]])(implicit mp: MathProcessor[N]): Matrix[N] = {
    Matrix(content map (_.toIndexedSeq map mp.fromInt))
  }

  def s[T](values: T*): IndexedSeq[T] = IndexedSeq(values: _*)

  def iv(ints: Int*): IntVec = IntVec((ints map BigInt.apply): _*)
  def nv[N <: MPNumber](ints: Int*)(implicit mp: MathProcessor[N]): NumVec[N] =
    NumVec[N]((ints map mp.fromInt): _*)
  def nv2[N <: MPNumber](fracs: Rational*)(implicit mp: MathProcessor[N]): NumVec[N] =
    NumVec[N]((fracs map mp.fromRational): _*)

  def frac(n: Int) = Rational(n, 1)
  def frac(n: Int, d: Int) = Rational(n, d)

  def makePoly[N <: MPNumber](components: (Int, Seq[Int])*)(implicit mp: MathProcessor[N]): Polynomial[N] =
    components map { case (coeff, pows) => Term(mp.fromInt(coeff), nv(pows: _*)) } toIndexedSeq

  val intCmp = new Comparator[Int] { override def compare(i1: Int, i2: Int) = i1 compare i2 }

  def markedTable(colsCount: Int, expectedVecs: Seq[IntVec], marked: IndexedSeq[IndexedSeq[Int]]): KeyTable[IntVec, Int, Boolean] = {
    val table1 = (0 until colsCount).foldLeft(KeyTable.empty[IntVec, Int, Boolean]) {
      case (table, i) => table.withEmptyCol(i)
    }
    val table2 = (expectedVecs zip marked).foldLeft(table1) {
      case (table, (vec, marked)) =>
        val table3 = marked.foldLeft(table) {
          case (table, i) => table + (vec, i, true)
        }
        table3.sortedCols
    }
    table2.sortedRows
  }

  /**
   * @param points sequence of [sequence of (pointIndices, upperLvlIndices)], one per dimension,
   *         in order of descending dimensinon. Last sequence will get the dimension of 0
   */
  def chainSurfaces(points: Seq[IndexedSeq[(Seq[Int], Seq[Int])]]): Map[Int, SortedSet[Surface]] = {
    def addToMap(
        acc:       Map[Int, SortedSet[Surface]],
        dim:       Int,
        current:   IndexedSeq[(Seq[Int], Seq[Int])],
        remaining: Seq[IndexedSeq[(Seq[Int], Seq[Int])]]
    ): Map[Int, SortedSet[Surface]] = {
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

  //
  // Assertions
  // (Sorry, found no way to form a pretty message in Scalatest 3.0.4)
  //

  implicit def toDoubleApproximateEquals[T1 <: DoubleConvertible](thisDouble: T1) = new {
    val eps = 0.000001

    /** This approximately equals that */
    def =~=[T2 <: DoubleConvertible](that: T2): Boolean = {
      (thisDouble.toDouble - that.toDouble).abs < eps
    }
  }
}
