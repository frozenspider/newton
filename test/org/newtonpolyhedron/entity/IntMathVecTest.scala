package org.newtonpolyhedron.entity

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.vector.IntMathVec

@RunWith(classOf[JUnitRunner])
class IntMathVecTest extends FunSuite {
  test("reducing") {
    val expected = IntMathVec(-1, 3, 5)
    val actual = IntMathVec(-2, 6, 10).reduced
    assert(actual === expected)
    assert(actual(0) === -1)
    assert(actual(1) === +3)
    assert(actual(2) === +5)
  }

  test("reducing sum") {
    val v1 = IntMathVec(-1, 3, 5)
    val v2 = IntMathVec(3, 1, 3)
    val actualSum = (v1 + v2).reduced
    assert(actualSum === IntMathVec(1, 2, 4))
  }

  test("+") {
    val v1 = IntMathVec(100, 200, 300)
    val v2 = IntMathVec(300, 200, 100)
    assert(v1 + v2 === IntMathVec(400, 400, 400))
  }

  test("-") {
    val v1 = IntMathVec(100, 200, 300)
    val v2 = IntMathVec(300, 200, 100)
    assert(v1 - v2 === IntMathVec(-200, 0, 200))
  }

  test("*") {
    val v1 = IntMathVec(2, 5, 6)
    val v2 = IntMathVec(5, 3, 5)
    assert(v1 * v2 === IntMathVec(10, 15, 30))
  }

  test("*+") {
    val v1 = IntMathVec(8, 3, 4)
    val v2 = IntMathVec(2, 2, 3)
    assert(v1 *+ v2 === (16 + 6 + 12))
  }
}