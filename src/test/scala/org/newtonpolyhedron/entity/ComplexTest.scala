package org.newtonpolyhedron.entity

import org.newtonpolyhedron.test._

import org.apache.commons.math3.fraction.BigFraction
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ComplexTest extends FunSuite {

  test("+-*/") {
    val c1 = Complex(1, 2)
    val c2 = Complex(3, -4)
    assert(c1 + c2 === Complex(4, -2))
    assert(c1 - c2 === Complex(-2, 6))
    assert(c1 * c2 === Complex(11, 2))
    assert(c1 / c2 === Complex(bf(-1, 5), bf(2, 5)))
    import Complex._
    assert(c1 + ZERO === c1)
    assert(c1 - ZERO === c1)
    assert(c1 * ZERO === ZERO)
    assert(ZERO * ZERO === ZERO)
    assert(ZERO / c1 === ZERO)
    intercept[IllegalArgumentException](c1 / ZERO)
  }

  test("power, inverse") {
    val c = Complex(3, -5)
    assert((c pow 0) === Complex(1, 0))
    assert((c pow 1) === c)
    assert((c pow -1) === Complex(bf(3, 34), bf(5, 34)))
    assert(c.inv === (c pow -1))
    assert((c pow 2) === Complex(-16, -30))
    assert((c pow 3) === Complex(-198, -10))
    assert((c pow 4) === Complex(-644, 960))
    assert((c pow 5) === Complex(2868, 6100))
    assert((c pow -4) === Complex(bf(-161, 334084), bf(-60, 83521)))
    assert((c pow -5) === Complex(bf(717, 11358856), bf(-1525, 11358856)))
    assert((c pow 8) === Complex(-506864, -1236480))
  }
}
