package org.newtonpolyhedron.solve.polyinter

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.solve.cone.ConeSolverImpl
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec

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
      (iv(-4, -3, -6) -> s(s(1, 0), s(1, 1), s(3, 0), s(3, 1))),
      (iv(-4, -3, -3) -> s(s(2, 0), s(2, 1), s(3, 0), s(3, 1))),
      (iv(-1, 0, 0) -> s(s(1, 1), s(1, 2), s(2, 1), s(2, 2))),
      (iv(0, -1, 0) -> s(s(0, 0), s(0, 2), s(2, 0), s(2, 2))),
      (iv(0, 0, -1) -> s(s(0, 0), s(0, 1), s(1, 0), s(1, 1))),
      (iv(8, 9, 5) -> s(s(0, 1), s(0, 3), s(1, 1), s(1, 3))),
      (iv(24, 27, 26) -> s(s(0, 2), s(0, 3), s(1, 2), s(1, 3))))

    val actual = solver.solve(source, 3)
    assert(actual === expected);
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
}