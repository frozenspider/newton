package org.newtonpolyhedron.entity

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.apache.commons.math3.fraction.BigFraction
import spire.math.Rational

@RunWith(classOf[JUnitRunner])
class RationalTest extends FunSuite {
  test("rounding") {
    // Note that rounding for math.round and Rational.round behaves differently for values in form -X.5
    val values = List(0, 3, 8.0, 8.1, 8.4, 8.49, 8.5, 8.51, 8.6, -4.0, -4.2, -4.49, -0.4, -4.4, -4.8, -4.99)
    for (value <- values) {
      val expected = math.round(value)
      val actual = Rational(value).round
      assert(expected === actual.longValue, s"for $value")
    }
  }
}
