package org.newtonpolyhedron.math.internal

import org.apache.commons.math3.exception.DimensionMismatchException
import org.junit.runner.RunWith
import org.newtonpolyhedron.test._
import org.newtonpolyhedron.NewtonImports._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ApacheMatrixMathTest
    extends FunSuite
    with InternalMathProcessorMixin {

  test("+ - *") {
    val mat1 = matrNum(s(
      s(1, 2, 3),
      s(2, 4, 6)
    ))
    val mat2 = matrNum(s(
      s(3, 3, 3),
      s(5, 5, 5)
    ))
    assert(mat1 + mat2
      === matrNum(s(
        s(4, 5, 6),
        s(7, 9, 11)
      )))
    assert(mat1 - mat2
      === matrNum(s(
        s(-2, -1, 0),
        s(-3, -1, 1)
      )))
    assert(mat1.negate
      === matrNum(s(
        s(-1, -2, -3),
        s(-2, -4, -6)
      )))
    assert(mat1.negate
      === matrNum(s(
        s(-1, -2, -3),
        s(-2, -4, -6)
      )))
    intercept[DimensionMismatchException] { mat1 * mat2 }

    val mat3 = matrNum(s(
      s(1, 2),
      s(2, 3),
      s(3, 6)
    ))
    assert(mat1 * mat3
      === matrNum(s(
        s(14, 26),
        s(28, 52)
      )))
    assert(mat3 * mat1
      === matrNum(s(
        s(5, 10, 15),
        s(8, 16, 24),
        s(15, 30, 45)
      )))
  }

  test("inverse") {
    assert(matrNum(s(
      s(1, 0, 0),
      s(-3, 1, 0),
      s(0, 0, 1)
    )).inverse
      ===
      matrNum(s(
        s(1, 0, 0),
        s(3, 1, 0),
        s(0, 0, 1)
      )))

    assert(matrNum(s(
      s(+1, 0, 0),
      s(-3, 1, 0),
      s(+0, 0, 1)
    )).inverse
      ===
      matrNum(s(
        s(1, 0, 0),
        s(3, 1, 0),
        s(0, 0, 1)
      )))

    assert(matrNum(s(
      s(1, -3, +2),
      s(0, +1, -2),
      s(0, +0, +1)
    )).inverse
      ===
      matrNum(s(
        s(1, 3, 4),
        s(0, 1, 2),
        s(0, 0, 1)
      )))
  }

  test("triangle form - no swaps") {
    assert(matrNum(s(
      s(1, 2, 3),
      s(2, 5, 8),
      s(3, 7, 12)
    )).triangleForm
      === (matrNum(s(
        s(1, 2, 3),
        s(0, 1, 2),
        s(0, 0, 1)
      )), 1))
  }

  test("triangle form - single swap") {
    val mat = matrNum(s(
      s(1, 2, 3, 3),
      s(2, 4, 8, 10),
      s(3, 7, 12, 13),
      s(3, 7, 11, 13)
    ))
    // Step 1
    //   {1, 2, 3, 3}
    //   {0, 0, 2, 4}
    //   {0, 1, 3, 4}
    //   {0, 1, 2, 4}

    // Step 2
    //   {1, 2, 3, 3}
    //   {0, 1, 3, 4}
    //   {0, 0, 2, 4}
    //   {0, 1, 2, 4}

    // Step 3
    //   {1, 2, 3, 3}
    //   {0, 1, 3, 4}
    //   {0, 0, 2, 4}
    //   {0, 0, -1, 0}
    assert(mat.triangleForm
      === (matrNum(s(
        s(1, 2, 3, 3),
        s(0, 1, 3, 4),
        s(0, 0, 2, 4),
        s(0, 0, 0, 2)
      )), -1))
  }

  test("determinant") {
    assert(matrNum(s(
      s(4, 6, 6),
      s(4, 5, 5),
      s(4, 7, 9)
    )).det === mp.fromInt(-8))

    assert(matrNum(s(
      s(1605, -1551, -3586),
      s(2278, -2267, -5287),
      s(2275, -2265, -5283)
    )).det === mp.fromInt(1))
  }

  test("rank") {
    assert(matrNum(s(s(1))).rank === 1)

    assert(matrNum(s(
      s(1, 2),
      s(3, 4)
    )).rank === 2)

    assert(matrNum(s(
      s(1, 2, 3),
      s(3, 4, 5)
    )).rank === 2)

    assert(matrNum(s(
      s(1, 2, 3),
      s(3, 4, 5),
      s(3, 4, 5)
    )).rank === 2)

    assert(matrNum(s(
      s(1, 2, 3),
      s(3, 4, 5),
      s(2, 4, 6)
    )).rank === 2)

    assert(matrNum(s(
      s(1, 2, 3),
      s(3, 4, 5),
      s(2, 4, 7)
    )).rank === 3)

    // Here non-integral division is involved
    assert(matrNum(s(
      s(-3, 1, 1),
      s(-1, 1, 0),
      s(-1, 0, 1)
    )).rank === 3)
  }
}
