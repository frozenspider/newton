package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.Matrix

@RunWith(classOf[JUnitRunner])
class UnimodularMatrixMakerImplTest extends FunSuite {

  val maker = new UnimodularMatrixMakerImpl

  test("non-square") {
    intercept[IllegalArgumentException] {
      maker.unimodularFrom(Matrix.zero(1, 2))
    }
  }

  test("3x3 case 1") {
    val source = matrFrac(a(
      a(1, 3, 4),
      a(3, 4, 2),
      a(0, 0, 0)))
    val expected = matrFrac(a(
      a(1, 3, 4),
      a(3, 10, 14),
      a(0, 0, 1)))

    val actual = maker.unimodularFrom(source)
    assert(actual === expected)
  }

  test("3x3 case 2") {
    val source = matrFrac(a(
      a(36, 18, 72),
      a(5, 6, 12),
      a(2, 8, 16)))
    val expected = matrFrac(a(
      a(1605, -1551, -3586),
      a(2278, -2267, -5287),
      a(2275, -2265, -5283)))

    val actual = maker.unimodularFrom(source)
    assert(actual === expected)
  }
}