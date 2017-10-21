package org.newtonpolyhedron.entity.matrix

import org.apache.commons.math3.exception.DimensionMismatchException
import org.junit.runner.RunWith
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite {
  test("core") {
    val mat = matrInt(s(
      s(1, 2, 3),
      s(2, 4, 6)
    ))
    assert(mat.rowCount === 2)
    assert(mat.colCount === 3)
    assert(mat(0, 0) === BigInt(1))
    assert(mat(1, 2) === BigInt(6))
    intercept[IllegalArgumentException] { mat(-1, 0) }
    intercept[IllegalArgumentException] { mat(0, -1) }
    intercept[IllegalArgumentException] { mat(0, 3) }
    intercept[IllegalArgumentException] { mat(1, 3) }
    intercept[IllegalArgumentException] { mat(2, 0) }
  }

  test("+ - *") {
    val mat1 = matrInt(s(
      s(1, 2, 3),
      s(2, 4, 6)
    ))
    val mat2 = matrInt(s(
      s(3, 3, 3),
      s(5, 5, 5)
    ))
    assert(mat1 + mat2
      === matrInt(s(
        s(4, 5, 6),
        s(7, 9, 11)
      )))
    assert(mat1 - mat2
      === matrInt(s(
        s(-2, -1, 0),
        s(-3, -1, 1)
      )))
    assert(-mat1
      === matrInt(s(
        s(-1, -2, -3),
        s(-2, -4, -6)
      )))
    assert(-mat1
      === matrInt(s(
        s(-1, -2, -3),
        s(-2, -4, -6)
      )))
    intercept[DimensionMismatchException] { mat1 * mat2 }

    val mat3 = matrInt(s(
      s(1, 2),
      s(2, 3),
      s(3, 6)
    ))
    assert(mat1 * mat3
      === matrInt(s(
        s(14, 26),
        s(28, 52)
      )))
    assert(mat3 * mat1
      === matrInt(s(
        s(5, 10, 15),
        s(8, 16, 24),
        s(15, 30, 45)
      )))
  }

  test("inverse") {
    assert(matrInt(s(
      s(1, 0, 0),
      s(-3, 1, 0),
      s(0, 0, 1)
    )).inv
      ===
      matrInt(s(
        s(1, 0, 0),
        s(3, 1, 0),
        s(0, 0, 1)
      )))

    assert(matrInt(s(
      s(+1, 0, 0),
      s(-3, 1, 0),
      s(+0, 0, 1)
    )).inv
      ===
      matrInt(s(
        s(1, 0, 0),
        s(3, 1, 0),
        s(0, 0, 1)
      )))

    assert(matrInt(s(
      s(1, -3, +2),
      s(0, +1, -2),
      s(0, +0, +1)
    )).inv
      ===
      matrInt(s(
        s(1, 3, 4),
        s(0, 1, 2),
        s(0, 0, 1)
      )))
  }

  test("transpose") {
    assert(matrInt(s(
      s(1, 2, 3),
      s(4, 5, 6)
    )).transpose
      ===
      matrInt(s(
        s(1, 4),
        s(2, 5),
        s(3, 6)
      )))
  }

  test("triangle form - no swaps") {
    assert(matrInt(s(
      s(1, 2, 3),
      s(2, 5, 8),
      s(3, 7, 12)
    )).triangleForm
      === (matrInt(s(
        s(1, 2, 3),
        s(0, 1, 2),
        s(0, 0, 1)
      )), 1))
  }

  test("triangle form - single swap") {
    val mat = matrFrac(s(
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
      === (matrFrac(s(
        s(1, 2, 3, 3),
        s(0, 1, 3, 4),
        s(0, 0, 2, 4),
        s(0, 0, 0, 2)
      )), -1))
  }

  test("determinant") {
    assert(matrInt(s(
      s(4, 6, 6),
      s(4, 5, 5),
      s(4, 7, 9)
    )).det === BigInt(-8))

    assert(matrInt(s(
      s(1605, -1551, -3586),
      s(2278, -2267, -5287),
      s(2275, -2265, -5283)
    )).det === BigInt(1))
  }

  test("rank") {
    assert(matrInt(s(s(1))).rank === 1)

    assert(matrInt(s(
      s(1, 2),
      s(3, 4)
    )).rank === 2)

    assert(matrInt(s(
      s(1, 2, 3),
      s(3, 4, 5)
    )).rank === 2)

    assert(matrInt(s(
      s(1, 2, 3),
      s(3, 4, 5),
      s(3, 4, 5)
    )).rank === 2)

    assert(matrInt(s(
      s(1, 2, 3),
      s(3, 4, 5),
      s(2, 4, 6)
    )).rank === 2)

    assert(matrInt(s(
      s(1, 2, 3),
      s(3, 4, 5),
      s(2, 4, 7)
    )).rank === 3)

    // Here non-integral division is involved, so we're using fractions
    assert(matrFrac(s(
      s(-3, 1, 1),
      s(-1, 1, 0),
      s(-1, 0, 1)
    )).rank === 3)
  }

  test("adding rows and cols") {
    val source = matrFrac(s(s(1)))
    val withRow1 = source addRow Seq(2)
    assert(withRow1
      === matrFrac(s(
        s(1),
        s(2)
      )))
    val withRow2 = withRow1 addRow Seq(3)
    assert(withRow2
      === matrFrac(s(
        s(1),
        s(2),
        s(3)
      )))
    val withCol1 = withRow2 addCol Seq(4, 5, 6)
    assert(withCol1
      === matrFrac(s(
        s(1, 4),
        s(2, 5),
        s(3, 6)
      )))
    val withRow3 = withCol1 addRow Seq(7, 8)
    assert(withRow3
      === matrFrac(s(
        s(1, 4),
        s(2, 5),
        s(3, 6),
        s(7, 8)
      )))
  }
}
