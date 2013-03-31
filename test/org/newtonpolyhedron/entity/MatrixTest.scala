package org.newtonpolyhedron.entity

import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.apache.commons.math3.exception.DimensionMismatchException

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite {
  private def matrInt(content: Array[Array[Int]]): Matrix[BigIntFielded] = {
    Matrix(content map (_ map (x => BigIntFielded(x))))
  }

  private def matrFrac(content: Array[Array[Int]]): Matrix[BigFrac] = {
    Matrix(content map (_ map (x => BigFrac(x))))
  }

  def a(values: Int*): Array[Int] = Array[Int](values: _*)
  def a(values: Array[Int]*): Array[Array[Int]] = Array[Array[Int]](values: _*)

  test("core") {
    val mat = matrInt(a(
      a(1, 2, 3),
      a(2, 4, 6)))
    assert(mat.rowNum === 2)
    assert(mat.colNum === 3)
    assert(mat(0, 0) === BigIntFielded(1))
    assert(mat(1, 2) === BigIntFielded(6))
    intercept[IllegalArgumentException] { mat(-1, 0) }
    intercept[IllegalArgumentException] { mat(0, -1) }
    intercept[IllegalArgumentException] { mat(0, 3) }
    intercept[IllegalArgumentException] { mat(1, 3) }
    intercept[IllegalArgumentException] { mat(2, 0) }
  }

  test("+ - *") {
    val mat1 = matrInt(a(
      a(1, 2, 3),
      a(2, 4, 6)))
    val mat2 = matrInt(a(
      a(3, 3, 3),
      a(5, 5, 5)))
    assert(mat1 + mat2
      === matrInt(a(
        a(4, 5, 6),
        a(7, 9, 11))))
    assert(mat1 - mat2
      === matrInt(a(
        a(-2, -1, 0),
        a(-3, -1, 1))))
    assert(-mat1
      === matrInt(a(
        a(-1, -2, -3),
        a(-2, -4, -6))))
    assert(-mat1
      === matrInt(a(
        a(-1, -2, -3),
        a(-2, -4, -6))))
    intercept[DimensionMismatchException] { mat1 * mat2 }

    val mat3 = matrInt(a(
      a(1, 2),
      a(2, 3),
      a(3, 6)))
    assert(mat1 * mat3
      === matrInt(a(
        a(14, 26),
        a(28, 52))))
    assert(mat3 * mat1
      === matrInt(a(
        a(5, 10, 15),
        a(8, 16, 24),
        a(15, 30, 45))))
  }

  test("inverse") {
    assert(matrInt(a(
      a(1, 0, 0),
      a(-3, 1, 0),
      a(0, 0, 1))).inv
      ===
      matrInt(a(
        a(1, 0, 0),
        a(3, 1, 0),
        a(0, 0, 1))))

    assert(matrInt(a(
      a(+1, 0, 0),
      a(-3, 1, 0),
      a(+0, 0, 1))).inv
      ===
      matrInt(a(
        a(1, 0, 0),
        a(3, 1, 0),
        a(0, 0, 1))))

    assert(matrInt(a(
      a(1, -3, +2),
      a(0, +1, -2),
      a(0, +0, +1))).inv
      ===
      matrInt(a(
        a(1, 3, 4),
        a(0, 1, 2),
        a(0, 0, 1))))
  }

  test("transpose") {
    assert(matrInt(a(
      a(1, 2, 3),
      a(4, 5, 6))).transpose
      ===
      matrInt(a(
        a(1, 4),
        a(2, 5),
        a(3, 6))))
  }

  test("triangle form - no swaps") {
    assert(matrInt(a(
      a(1, 2, 3),
      a(2, 5, 8),
      a(3, 7, 12))).triangleForm
      === (matrInt(a(
        a(1, 2, 3),
        a(0, 1, 2),
        a(0, 0, 1))),
        1))
  }

  test("triangle form - single swap") {
    val mat = matrInt(a(
      a(1, 2, 3, 3),
      a(2, 4, 8, 10),
      a(3, 7, 12, 13),
      a(3, 7, 11, 13)))
    // Step 1
    // 	{1, 2, 3, 3}
    // 	{0, 0, 2, 4}
    // 	{0, 1, 3, 4}
    // 	{0, 1, 2, 4}

    // Step 2
    // 	{1, 2, 3, 3}
    // 	{0, 1, 3, 4}
    // 	{0, 0, 2, 4}
    // 	{0, 1, 2, 4}

    // Step 3
    // 	{1, 2, 3, 3}
    // 	{0, 1, 3, 4}
    // 	{0, 0, 2, 4}
    // 	{0, 0, -1, 0}
    assert(mat.triangleForm
      === (matrInt(a(
        a(1, 2, 3, 3),
        a(0, 1, 3, 4),
        a(0, 0, 2, 4),
        a(0, 0, 0, 2))),
        -1))
  }

  test("determinant") {
    assert(matrInt(a(
      a(4, 6, 6),
      a(4, 5, 5),
      a(4, 7, 9))).det
      === BigFrac(-8))

    assert(matrInt(a(
      a(1605, -1551, -3586),
      a(2278, -2267, -5287),
      a(2275, -2265, -5283))).det
      === BigFrac(1))
  }

  test("rank") {
    assert(matrInt(a(a(1))).rank === 1)

    assert(matrInt(a(
      a(1, 2),
      a(3, 4))).rank
      === 2)

    assert(matrInt(a(
      a(1, 2, 3),
      a(3, 4, 5))).rank
      === 2)

    assert(matrInt(a(
      a(1, 2, 3),
      a(3, 4, 5),
      a(3, 4, 5))).rank
      === 2)

    assert(matrInt(a(
      a(1, 2, 3),
      a(3, 4, 5),
      a(2, 4, 6))).rank
      === 2)

    assert(matrInt(a(
      a(1, 2, 3),
      a(3, 4, 5),
      a(2, 4, 7))).rank
      === 3)

    assert(matrInt(a(
      a(-3, 1, 1),
      a(-1, 1, 0),
      a(-1, 0, 1))).rank
      === 3)
  }

  test("diagonal form 1") {
    val source = matrFrac(a(
      a(0, 1, -1),
      a(2, -3, 0),
      a(0, 0, 0)))
    val (actual, rowOnes, colOnes) = Matrix.toDiagonal(source)
    assert(actual
      === matrFrac(a(
        a(1, 0, 0),
        a(0, -1, 0),
        a(0, 0, 0))))
    assert(rowOnes
      === matrFrac(a(
        a(1, 0, 0),
        a(3, 1, 0),
        a(0, 0, 1))))
    assert(colOnes
      === matrFrac(a(
        a(0, 1, 3),
        a(1, 1, 2),
        a(0, 1, 2))))
  }

  test("diagonal form 2") {
    val source = matrFrac(a(
      a(36, 18, 72),
      a(5, 6, 12),
      a(2, 8, 16)))
    val (actual, rowOnes, colOnes) = Matrix.toDiagonal(source)
    assert(actual
      === matrFrac(a(
        a(1, 0, 0),
        a(0, 2, 0),
        a(0, 0, 504))))
    assert(rowOnes
      === matrFrac(a(
        a(0, -1, 1),
        a(-3, 142, -140),
        a(-14, 666, -657))))
    assert(colOnes
      === matrFrac(a(
        a(-1, 2, -108),
        a(-1, 9, -484),
        a(0, -3, 161))))
  }
}