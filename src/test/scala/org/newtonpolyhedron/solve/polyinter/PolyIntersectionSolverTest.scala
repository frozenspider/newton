package org.newtonpolyhedron.solve.polyinter

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table.KeyTable
import org.junit.runner.RunWith
import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.solve.cone.MotzkinBurger
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PolyIntersectionSolverTest
    extends FunSuite
    with InternalMathProcessorMixin {

  val solver = new PolyIntersectionSolverImpl(new MotzkinBurger)

  test("simple") {
    val source = s(
      s(
        nv(9, 0, 0),
        nv(0, 8, 0),
        nv(0, 0, 7),
        nv(3, 2, 1)
      ),
      s(
        nv(3, 0, 0),
        nv(0, 4, 0),
        nv(0, 0, 5),
        nv(1, 2, 2)
      )
    )
    val expected = Map(
      (iv(-4, -3, -6) -> s(s(1, 3), s(0, 1))),
      (iv(-4, -3, -3) -> s(s(2, 3), s(0, 1))),
      (iv(-1, 0, 0) -> s(s(1, 2), s(1, 2))),
      (iv(0, -1, 0) -> s(s(0, 2), s(0, 2))),
      (iv(0, 0, -1) -> s(s(0, 1), s(0, 1))),
      (iv(8, 9, 5) -> s(s(0, 1), s(1, 3))),
      (iv(24, 27, 26) -> s(s(0, 1), s(2, 3)))
    )

    val expectedTable = polyTableFromMap(expected)

    val actual = solver.solve(source, 3)
    assert(actual === expectedTable)
  }

  test("Orkin, 2013-06-08") {
    val source = s(
      s(
        nv(1, 1, 1),
        nv(6, 0, 0),
        nv(0, 6, 0),
        nv(0, 0, 6),
        nv(3, 0, 3)
      ),
      s(
        nv(0, 1, 1),
        nv(2, 0, 1),
        nv(3, 1, 0),
        nv(2, 1, 1)
      )
    )
    val expected = Map(
      (iv(-5, -10, -3) -> s(s(0, 3), s(0, 1))),
      (iv(-5, -4, -15) -> s(s(0, 2), s(0, 2))),
      (iv(-1, -2, -3) -> s(s(0, 1), s(0, 1, 2))),
      (iv(0, 1, 1) -> s(s(2, 3), s(0, 3))),
      (iv(1, 0, 1) -> s(s(1, 3, 4), s(1, 2, 3))),
      (iv(1, 1, 1) -> s(s(1, 2, 3, 4), s(2, 3)))
    )
    val expectedTable = polyTableFromMap(expected)

    val actual = solver.solve(source, 3)
    assert(actual === expectedTable)
  }

  test("unknown example") {
    val source = s(
      s(
        nv(4, 0, 0),
        nv(0, 4, 0),
        nv(0, 2, 2),
        nv(0, 1, 3),
        nv(0, 0, 4),
        nv(1, 2, 2)
      ),
      s(
        nv(2, 0, 0),
        nv(0, 3, 0),
        nv(0, 0, 4),
        nv(1, 1, 2)
      )
    )
    val expected = Map(
      (iv(-3, -2, -2) -> s(s(1, 2, 3, 4), s(0, 1))),
      // ^ This intersection is dominating and is actually useful, may require this info later.
      // Besides, there are actually four points of the same line - p2 and p3 are inner points
      (iv(-1, 0, 0) -> s(s(1, 2, 3, 4), s(1, 2))),
      (iv(0, -1, 0) -> s(s(0, 4), s(0, 2))),
      (iv(0, 0, -1) -> s(s(0, 1), s(0, 1))),
      (iv(2, 2, 1) -> s(s(0, 1, 5), s(1, 3))),
      (iv(2, 4, 3) -> s(s(1, 5), s(1, 2, 3))),
      (iv(6, 4, 5) -> s(s(0, 5), s(2, 3)))
    )

    val expectedTable = polyTableFromMap(expected)

    val actual = solver.solve(source, 3)
    assert(actual === expectedTable)
  }

  // TODO: More tests

  //
  // Solution filtering tests
  //

  test("filter out non-intersecting solutions, case 1") {
    val eqSystems = s(
      s(
        iv(-9, 8, 0),
        iv(-9, 0, 7),
        iv(-6, 2, 1)
      ),
      s(
        iv(-3, 4, 0),
        iv(-3, 0, 5),
        iv(-2, 2, 2)
      )
    )
    val solutions = s(
      iv(-8, -9, -30),
      iv(-14, -33, -18),
      iv(0, 0, -1),
      iv(0, -1, 0),
      iv(4, 3, 1),
      iv(5, 2, 3)
    )
    val expected = s(
      iv(0, 0, -1),
      iv(0, -1, 0)
    ).sorted

    val filtered = solutions.filter(solver.isIntersectingSol(eqSystems)).sorted
    assert(filtered === expected)
  }

  test("filter out non-intersecting solutions, case 2") {
    val eqSystems = s(
      s(
        iv(9, -8, 0),
        iv(0, -8, 7),
        iv(3, -6, 1)
      ),
      s(
        iv(3, -4, 0),
        iv(0, -4, 5),
        iv(1, -2, 2)
      )
    )
    val solutions = s(
      iv(-34, -21, -24),
      iv(0, 0, -1),
      iv(-4, -3, -6),
      iv(-1, 0, 0),
      iv(8, 9, 5),
      iv(2, 5, 4)
    )
    val expected = s(
      iv(0, 0, -1),
      iv(-4, -3, -6),
      iv(-1, 0, 0),
      iv(8, 9, 5)
    ).sorted

    val filtered = solutions.filter(solver.isIntersectingSol(eqSystems)).sorted
    assert(filtered === expected)
  }

  test("filter out non-intersecting solutions, case 3") {
    val eqSystems = s(
      s(
        iv(9, 0, -7),
        iv(0, 8, -7),
        iv(3, 2, -6)
      ),
      s(
        iv(3, 0, -5),
        iv(0, 4, -5),
        iv(1, 2, -3)
      )
    )
    val solutions = s(
      iv(56, 63, 72),
      iv(0, -1, 0),
      iv(-1, 0, 0),
      iv(-20, -15, -12)
    )
    val expected = s(
      iv(0, -1, 0),
      iv(-1, 0, 0)
    ).sorted

    val filtered = solutions.filter(solver.isIntersectingSol(eqSystems)).sorted
    assert(filtered === expected)
  }

  /** @param map { vector -> [ pts sequence per polyhedron ] }*/
  private def polyTableFromMap(map: Map[IntVec, IndexedSeq[Seq[Int]]]): KeyTable[Int, IntVec, SortedSet[Int]] = {
    val map2 = map mapValues {
      case seq => seq.mapWithIndex(SortedSet(_: _*) -> _).map(_.swap).toMap
    }
    val table = KeyTable.fromRows(map2).transpose
    table.sortedCols
  }
}
