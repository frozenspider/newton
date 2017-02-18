package org.newtonpolyhedron.entity

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.apache.commons.math3.fraction.BigFraction

@RunWith(classOf[JUnitRunner])
class BigFracTest extends FunSuite {
  test("rounding") {
    val values = List(0, 3, 8.0, 8.1, 8.4, 8.49, 8.5, 8.51, 8.6, -4.0, -4.2, -4.49, -0.5, -4.5, -4.8, -4.99)
    for (value <- values) {
      val expected = math.round(value)
      val actual = new BigFrac(new BigFraction(value)).round
      assert(expected === actual.longValue)
    }
  }
}
