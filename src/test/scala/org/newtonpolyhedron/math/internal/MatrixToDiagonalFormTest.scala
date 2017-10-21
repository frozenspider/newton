package org.newtonpolyhedron.math.internal

import MatrixToDiagonalForm._
import org.junit.runner.RunWith
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixToDiagonalFormTest extends FunSuite {

  test("diagonal form 1") {
    val source = matrFrac(s(
      s(0, 1, -1),
      s(2, -3, 0),
      s(0, 0, 0)
    ))
    val (actual, rowOnes, colOnes) = toDiagonal(source)
    assert(actual
      === matrFrac(s(
        s(1, 0, 0),
        s(0, -1, 0),
        s(0, 0, 0)
      )))
    assert(rowOnes
      === matrFrac(s(
        s(1, 0, 0),
        s(3, 1, 0),
        s(0, 0, 1)
      )))
    assert(colOnes
      === matrFrac(s(
        s(0, 1, 3),
        s(1, 1, 2),
        s(0, 1, 2)
      )))
  }

  test("diagonal form 2") {
    val source = matrFrac(s(
      s(36, 18, 72),
      s(5, 6, 12),
      s(2, 8, 16)
    ))
    val (actual, rowOnes, colOnes) = toDiagonal(source)
    assert(actual
      === matrFrac(s(
        s(1, 0, 0),
        s(0, 2, 0),
        s(0, 0, 504)
      )))
    assert(rowOnes
      === matrFrac(s(
        s(0, -1, 1),
        s(-3, 142, -140),
        s(-14, 666, -657)
      )))
    assert(colOnes
      === matrFrac(s(
        s(-1, 2, -108),
        s(-1, 9, -484),
        s(0, -3, 161)
      )))
  }
}
