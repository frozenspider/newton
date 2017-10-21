package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.NewtonImports._

@RunWith(classOf[JUnitRunner])
class UnimodularMatrixMakerImplTest
    extends FunSuite
    with InternalMathProcessorMixin {

  val maker = new UnimodularMatrixMakerImpl

  test("non-square") {
    intercept[IllegalArgumentException] {
      maker.unimodularFrom(Matrix.zero[N](1, 2))
    }
  }

  test("3x3 case 1") {
    val source = matrNum(s(
      s(1, 3, 4),
      s(3, 4, 2),
      s(0, 0, 0)
    ))
    val expected = matrNum(s(
      s(1, 3, 4),
      s(3, 10, 14),
      s(0, 0, 1)
    ))

    val actual = maker.unimodularFrom(source)
    assert(actual === expected)
  }

  test("3x3 case 2") {
    val source = matrNum(s(
      s(36, 18, 72),
      s(5, 6, 12),
      s(2, 8, 16)
    ))
    val expected = matrNum(s(
      s(1605, -1551, -3586),
      s(2278, -2267, -5287),
      s(2275, -2265, -5283)
    ))

    val actual = maker.unimodularFrom(source)
    assert(actual === expected)
  }

  test("3x3 power transform article") {
    // Article by Sollev and Khakimov
    val source = matrNum(s(
      s(2, -3, 1),
      s(-1, -1, 1),
      s(1, -1, 0)
    ))
    // Unimodular matrix in article:
    //  1, -1, -2
    //  1, -1, -3
    //  2, -1, -5
    // Unimodular matrix computed by app:
    // -2,  3, -1
    //  1,  1, -1
    //  1, -1,  0
    val expected = matrNum(s(
      s(-2, 3, -1),
      s(1, 1, -1),
      s(1, -1, 0)
    ))

    val actual = maker.unimodularFrom(source)
    assert(actual === expected)
  }
}
