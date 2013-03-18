package org.newtonpolyhedron.entity

import org.scalatest.FunSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.apache.commons.math3.exception.DimensionMismatchException

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite {
  private def matr(content: Array[Array[Int]]): Matrix[BigFrac] = {
    Matrix(content map (_ map (x => BigFrac(x))))
  }
  def a(values: Int*): Array[Int] = Array[Int](values: _*)
  def a(values: Array[Int]*): Array[Array[Int]] = Array[Array[Int]](values: _*)

  test("core") {
    val mat = matr(a(
      a(1, 2, 3),
      a(2, 4, 6)))
    assert(mat.rowNum === 2)
    assert(mat.colNum === 3)
    assert(mat(0, 0) === BigFrac(1))
    assert(mat(1, 2) === BigFrac(6))
    intercept[IllegalArgumentException] { mat(-1, 0) }
    intercept[IllegalArgumentException] { mat(0, -1) }
    intercept[IllegalArgumentException] { mat(0, 3) }
    intercept[IllegalArgumentException] { mat(1, 3) }
    intercept[IllegalArgumentException] { mat(2, 0) }
  }

  test("+ - *") {
    val mat1 = matr(a(
      a(1, 2, 3),
      a(2, 4, 6)))
    val mat2 = matr(a(
      a(3, 3, 3),
      a(5, 5, 5)))
    assert(mat1 + mat2
      === matr(a(
        a(4, 5, 6),
        a(7, 9, 11))))
    assert(mat1 - mat2
      === matr(a(
        a(-2, -1, 0),
        a(-3, -1, 1))))
    assert(-mat1
      === matr(a(
        a(-1, -2, -3),
        a(-2, -4, -6))))
    assert(-mat1
      === matr(a(
        a(-1, -2, -3),
        a(-2, -4, -6))))
    intercept[DimensionMismatchException] { mat1 * mat2 }

    val mat3 = matr(a(
      a(1, 2),
      a(2, 3),
      a(3, 6)))
    assert(mat1 * mat3
      === matr(a(
        a(14, 26),
        a(28, 52))))
    assert(mat3 * mat1
      === matr(a(
        a(5, 10, 15),
        a(8, 16, 24),
        a(15, 30, 45))))
  }

  test("inverse") {
    assert(matr(a(
      a(1, 0, 0),
      a(-3, 1, 0),
      a(0, 0, 1))).inv
      ===
      matr(a(
        a(1, 0, 0),
        a(3, 1, 0),
        a(0, 0, 1))))

    assert(matr(a(
      a(+1, 0, 0),
      a(-3, 1, 0),
      a(+0, 0, 1))).inv
      ===
      matr(a(
        a(1, 0, 0),
        a(3, 1, 0),
        a(0, 0, 1))))

    assert(matr(a(
      a(1, -3, +2),
      a(0, +1, -2),
      a(0, +0, +1))).inv
      ===
      matr(a(
        a(1, 3, 4),
        a(0, 1, 2),
        a(0, 0, 1))))
  }

  test("transpose") {
    assert(matr(a(
      a(1, 2, 3),
      a(4, 5, 6))).transpose
      ===
      matr(a(
        a(1, 4),
        a(2, 5),
        a(3, 6))))
  }

  test("triangle form - no swaps") {
    assert(matr(a(
      a(1, 2, 3),
      a(2, 5, 8),
      a(3, 7, 12))).triangleForm
      === (matr(a(
        a(1, 2, 3),
        a(0, 1, 2),
        a(0, 0, 1))),
        1))
  }

  test("triangle form - single swap") {
    val mat = matr(a(
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
      === (matr(a(
        a(1, 2, 3, 3),
        a(0, 1, 3, 4),
        a(0, 0, 2, 4),
        a(0, 0, 0, 2))),
        -1))
  }

  test("determinant") {
    assert(matr(a(
      a(4, 6, 6),
      a(4, 5, 5),
      a(4, 7, 9))).det
      === BigFrac(-8))

    assert(matr(a(
      a(1605, -1551, -3586),
      a(2278, -2267, -5287),
      a(2275, -2265, -5283))).det
      === BigFrac(1))
  }

  test("rank") {
    assert(matr(a(a(1))).rank === 1)

    assert(matr(a(
      a(1, 2),
      a(3, 4))).rank
      === 2)

    assert(matr(a(
      a(1, 2, 3),
      a(3, 4, 5))).rank
      === 2)

    assert(matr(a(
      a(1, 2, 3),
      a(3, 4, 5),
      a(3, 4, 5))).rank
      === 2)

    assert(matr(a(
      a(1, 2, 3),
      a(3, 4, 5),
      a(2, 4, 6))).rank
      === 2)

    assert(matr(a(
      a(1, 2, 3),
      a(3, 4, 5),
      a(2, 4, 7))).rank
      === 3)
  }
}