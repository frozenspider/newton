package org.newtonpolyhedron.entity

import org.newtonpolyhedron.test._

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PolynomialUtils._

@RunWith(classOf[JUnitRunner])
class PolynomialWrapperTest extends FunSuite {

  test("powers") {
    val `x + y` = makePoly((1, s(1, 0)), (1, s(0, 1)))
    val `(x + y)^0` = `x + y` pow 0
    val `(x + y)^1` = `x + y` pow 1
    val `(x + y)^2` = `x + y` pow 2
    val `1` = makePoly((1, s(0, 0)))
    val `x^2 + 2xy + y^2` = makePoly((1, s(2, 0)), (2, s(1, 1)), (1, s(0, 2)))
    assert(`(x + y)^1`.toSet === `x + y`.toSet)
    assert(`(x + y)^2`.toSet === `x^2 + 2xy + y^2`.toSet)
    assert(`(x + y)^0`.toSet === `1`.toSet)
  }

  test("skip zeros") {
    val `0x + x + 0xy + 0` = makePoly((0, s(1, 0)), (1, s(1, 0)), (0, s(1, 1)), (0, s(0, 0)))
    val `x` = makePoly((1, s(1, 0)))
    assert(`0x + x + 0xy + 0`.skipZeroTerms.toSet === `x`.toSet)
  }

  test("collapse dups") {
    val `x + 2x^2y + y + x^2y + 0` = makePoly((1, s(1, 0)), (2, s(2, 1)), (1, s(0, 1)), (1, s(2, 1)), (0, s(2, 1)))
    val `x + 3x^2y + y` = makePoly((1, s(1, 0)), (3, s(2, 1)), (1, s(0, 1)))
    assert(`x + 2x^2y + y + x^2y + 0`.collapseDups.toSet === `x + 3x^2y + y`.toSet)
  }

  test("multiplication") {
    val `x + 2y` = makePoly(
      (1, s(1, 0)),
      (2, s(0, 1))
    )
    val `2x + x^2 - y` =
      makePoly(
        (2, s(1, 0)),
        (1, s(2, 0)),
        (-1, s(0, 1))
      )
    val `2x^2 + x^3 + 3xy + 2x^2y - 2y^2` = makePoly(
      (2, s(2, 0)),
      (1, s(3, 0)),
      (3, s(1, 1)),
      (2, s(2, 1)),
      (-2, s(0, 2))
    )
    assert((`x + 2y` * `2x + x^2 - y`).toSet === `2x^2 + x^3 + 3xy + 2x^2y - 2y^2`.toSet)
  }
}
