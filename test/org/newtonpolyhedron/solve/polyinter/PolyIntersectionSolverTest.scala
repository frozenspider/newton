package org.newtonpolyhedron.solve.polyinter
import scala.collection.immutable.SortedSet
import org.fs.utils.collection.table.KeyTable
import org.junit.runner.RunWith
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.solve.cone.ConeSolverImpl
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTables

@RunWith(classOf[JUnitRunner])
class PolyIntersectionSolverTest extends FunSuite {

  val solver = new PolyIntersectionSolverImpl(new ConeSolverImpl)

  test("simple") {
    val source = s(
      s(
        fv(9, 0, 0),
        fv(0, 8, 0),
        fv(0, 0, 7),
        fv(3, 2, 1)),
      s(
        fv(3, 0, 0),
        fv(0, 4, 0),
        fv(0, 0, 5),
        fv(1, 2, 2)))
    val expected = Map(
      (iv(-4, -3, -6) -> s(s(1, 3), s(0, 1))),
      (iv(-4, -3, -3) -> s(s(2, 3), s(0, 1))),
      (iv(-1, 0, 0) -> s(s(1, 2), s(1, 2))),
      (iv(0, -1, 0) -> s(s(0, 2), s(0, 2))),
      (iv(0, 0, -1) -> s(s(0, 1), s(0, 1))),
      (iv(8, 9, 5) -> s(s(0, 1), s(1, 3))),
      (iv(24, 27, 26) -> s(s(0, 1), s(2, 3))))

    val expectedTable = polyTableFromMap(expected)

    val actual = solver.solve(source, 3)
    assert(actual === expectedTable)
  }

  // TODO: More tests

  test("filter out non-intersecting solutions, case 1") {
    val eqSystems = s(
      s(
        iv(-9, 8, 0),
        iv(-9, 0, 7),
        iv(-6, 2, 1)),
      s(
        iv(-3, 4, 0),
        iv(-3, 0, 5),
        iv(-2, 2, 2)))
    val solutions = s(
      iv(-8, -9, -30),
      iv(-14, -33, -18),
      iv(0, 0, -1),
      iv(0, -1, 0),
      iv(4, 3, 1),
      iv(5, 2, 3))
    val expected = s(
      iv(0, 0, -1),
      iv(0, -1, 0)).sorted

    val filtered = solutions.filter(solver.isIntersectingSol(eqSystems)).sorted
    assert(filtered === expected)
  }

  test("filter out non-intersecting solutions, case 2") {
    val eqSystems = s(
      s(
        iv(9, -8, 0),
        iv(0, -8, 7),
        iv(3, -6, 1)),
      s(
        iv(3, -4, 0),
        iv(0, -4, 5),
        iv(1, -2, 2)))
    val solutions = s(
      iv(-34, -21, -24),
      iv(0, 0, -1),
      iv(-4, -3, -6),
      iv(-1, 0, 0),
      iv(8, 9, 5),
      iv(2, 5, 4))
    val expected = s(
      iv(0, 0, -1),
      iv(-4, -3, -6),
      iv(-1, 0, 0),
      iv(8, 9, 5)).sorted

    val filtered = solutions.filter(solver.isIntersectingSol(eqSystems)).sorted
    assert(filtered === expected)
  }

  test("filter out non-intersecting solutions, case 3") {
    val eqSystems = s(
      s(
        iv(9, 0, -7),
        iv(0, 8, -7),
        iv(3, 2, -6)),
      s(
        iv(3, 0, -5),
        iv(0, 4, -5),
        iv(1, 2, -3)))
    val solutions = s(
      iv(56, 63, 72),
      iv(0, -1, 0),
      iv(-1, 0, 0),
      iv(-20, -15, -12))
    val expected = s(
      iv(0, -1, 0),
      iv(-1, 0, 0)).sorted

    val filtered = solutions.filter(solver.isIntersectingSol(eqSystems)).sorted
    assert(filtered === expected)
  }

  test("Orkin, 2013-06-08") {
    val source = s(
      s(
        fv(1, 1, 1),
        fv(6, 0, 0),
        fv(0, 6, 0),
        fv(0, 0, 6),
        fv(3, 0, 3)),
      s(
        fv(0, 1, 1),
        fv(2, 0, 1),
        fv(3, 1, 0),
        fv(2, 1, 1)))
    val expected = Map(
      (iv(-5, -10, -3) -> s(s(0, 3), s(0, 1))),
      (iv(-5, -4, -15) -> s(s(0, 2), s(0, 2))),
      (iv(-1, -2, -3) -> s(s(0, 1), s(0, 1, 2))),
      (iv(0, 1, 1) -> s(s(2, 3), s(0, 3))),
      (iv(1, 0, 1) -> s(s(1, 3, 4), s(1, 2, 3))),
      (iv(1, 1, 1) -> s(s(1, 2, 3, 4), s(2, 3))))
    val expectedTable = polyTableFromMap(expected)

    val actual = solver.solve(source, 3)
    assert(actual === expectedTable)
  }

  /** @param map { vector -> [ pts sequence per polyhedron ] }*/
  private def polyTableFromMap(map: Map[IntMathVec, IndexedSeq[Seq[Int]]]): KeyTable[Int, IntMathVec, SortedSet[Int]] = {
    var table = new ArrayListKeyTable[Int, IntMathVec, SortedSet[Int]]
    map foreach {
      case (vec, seq) =>
        (seq zip (0 until seq.size)) map {
          case (points, polyIdx) =>
            table.put(polyIdx, vec, SortedSet(points: _*))
        }
    }
    KeyTables.sortByColHeaders(table, true)
    table
  }
}
/*
Map(

[ -5 -10 -3 ] -> Vector(Vector(0, 0), Vector(0, 1), Vector(3, 0), Vector(3, 1)),
[ 1 0 1 ] -> Vector(Vector(1, 1), Vector(1, 2), Vector(1, 3), Vector(3, 1), Vector(3, 2), Vector(3, 3), Vector(4, 1), Vector(4, 2), Vector(4, 3)),
[ -5 -4 -15 ] -> Vector(Vector(0, 0), Vector(0, 2), Vector(2, 0), Vector(2, 2)),
[ 0 1 1 ] -> Vector(Vector(2, 0), Vector(2, 3), Vector(3, 0), Vector(3, 3)),
[ -1 -2 -3 ] -> Vector(Vector(0, 0), Vector(0, 1), Vector(0, 2), Vector(1, 0), Vector(1, 1), Vector(1, 2)),
[ 1 1 1 ] -> Vector(Vector(1, 2), Vector(1, 3), Vector(2, 2), Vector(2, 3), Vector(3, 2), Vector(3, 3), Vector(4, 2), Vector(4, 3)))


*/

