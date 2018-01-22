package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.math.PolynomialImports._
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.utils.NullPrintWriter

@RunWith(classOf[JUnitRunner])
class SimpleEqSystemSolverImplTest
    extends FunSuite
    with InternalMathProcessorMixin {

  val solver = new SimpleEqSystemSolverImpl

  test("simplest 2d example 1") {
    val eqSys = s(
      makePoly(
        (+1, s(-1, +0)),
        (-1, s(+0, +0))
      ),
      makePoly(
        (-1, s(+0, +0)),
        (-1, s(+0, +1))
      )
    )
    val actual = solver.solve(eqSys)

    val expected = s(1, -1) map mp.fromInt

    assert(actual.tail.isEmpty)
    assert(expected === actual.head)
  }

  test("simplest 2d example 2") {
    val eqSys = s(
      makePoly(
        (+1, s(+1, +0)),
        (-1, s(+0, +0))
      ),
      makePoly(
        (-1, s(+0, +0)),
        (-1, s(+4, +1))
      )
    )
    val actual = solver.solve(eqSys)

    val expected = s(1, -1) map mp.fromInt

    assert(actual.tail.isEmpty)
    assert(expected === actual.head)
  }

  test("simple 2d example 1") {
    val eqSys = s(
      makePoly(
        (+1, s(+1, +0)),
        (-2, s(+0, +0))
      ),
      makePoly(
        (-1, s(+0, +0)),
        (-1, s(+4, +1))
      )
    )
    val actual = solver.solve(eqSys)

    val expected = nv2(frac(2), frac(-1, 16))

    assert(actual.tail.isEmpty)
    assert(expected === actual.head)
  }

  test("simple 3d example 1") {
    // x1,x2,x3 = -1,3,2
    // 3 * x3 + 2 * x1x2 = 0
    // x1 * x2 * x3^2 - 6 * x1 * x3 = 0
    // 3 * x1^-1 + x2 = 0
    val eqSys = s(
      makePoly(
        (+3, s(+0, +0, +1)),
        (+2, s(+1, +1, +0))
      ),
      makePoly(
        (+1, s(+1, +1, +2)),
        (-6, s(+1, +0, +1))
      ),
      makePoly(
        (+3, s(-1, +0, +0)),
        (+1, s(+0, +1, +0))
      )
    )
    val actual = solver.solve(eqSys)

    val expected = s(-1, 3, 2) map mp.fromInt

    assert(actual.tail.isEmpty)
    assert(expected === actual.head)
  }
}
