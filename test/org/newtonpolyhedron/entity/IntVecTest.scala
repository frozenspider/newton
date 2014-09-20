package org.newtonpolyhedron.entity

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.vector.VectorImports._

@RunWith(classOf[JUnitRunner])
class IntVecTest extends FunSuite {
  test("reducing") {
    val expected = IntVec(-1, 3, 5)
    val actual = IntVec(-2, 6, 10).reduced
    assert(actual === expected)
    assert(actual(0) === -1)
    assert(actual(1) === +3)
    assert(actual(2) === +5)
  }

  test("reducing sum") {
    val v1 = IntVec(-1, 3, 5)
    val v2 = IntVec(3, 1, 3)
    val actualSum = (v1 + v2).reduced
    assert(actualSum === IntVec(1, 2, 4))
  }

  test("+") {
    val v1 = IntVec(100, 200, 300)
    val v2 = IntVec(300, 200, 100)
    assert(v1 + v2 === IntVec(400, 400, 400))
  }

  test("-") {
    val v1 = IntVec(100, 200, 300)
    val v2 = IntVec(300, 200, 100)
    assert(v1 - v2 === IntVec(-200, 0, 200))
  }

  test("*") {
    val v1 = IntVec(2, 5, 6)
    val v2 = IntVec(5, 3, 5)
    assert(v1 * v2 === IntVec(10, 15, 30))
  }

  test("*+") {
    val v1 = IntVec(8, 3, 4)
    val v2 = IntVec(2, 2, 3)
    assert(v1 *+ v2 === (16 + 6 + 12))
  }
}