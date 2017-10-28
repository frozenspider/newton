package org.newtonpolyhedron.entity.math

import org.junit.runner.RunWith
import org.newtonpolyhedron.test._
import org.newtonpolyhedron.NewtonImports._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrixTest
    extends FunSuite
    with InternalMathProcessorMixin {

  test("core") {
    val matData = s(
      s(1, 2, 3),
      s(2, 4, 6)
    )
    val matDataN = matData map (_ map mp.fromInt)
    val mat = matrNum(matData)
    assert(mat.rowCount === 2)
    assert(mat.colCount === 3)
    assert(mat(0, 0) === mp.fromInt(1))
    assert(mat(1, 2) === mp.fromInt(6))
    assert(mat.rows === matDataN)
    assert(mat.cols === matDataN.transpose)
    intercept[IllegalArgumentException] { mat(-1, 0) }
    intercept[IllegalArgumentException] { mat(0, -1) }
    intercept[IllegalArgumentException] { mat(0, 3) }
    intercept[IllegalArgumentException] { mat(1, 3) }
    intercept[IllegalArgumentException] { mat(2, 0) }
  }

  test("transpose") {
    assert(matrNum(s(
      s(1, 2, 3),
      s(4, 5, 6)
    )).transpose
      ===
      matrNum(s(
        s(1, 4),
        s(2, 5),
        s(3, 6)
      )))
  }

  test("adding rows and cols") {
    val source = matrNum(s(s(1)))
    val withRow1 = source addRow nv(2)
    assert(withRow1
      === matrNum(s(
        s(1),
        s(2)
      )))
    val withRow2 = withRow1 addRow nv(3)
    assert(withRow2
      === matrNum(s(
        s(1),
        s(2),
        s(3)
      )))
    val withCol1 = withRow2 addCol nv(4, 5, 6)
    assert(withCol1
      === matrNum(s(
        s(1, 4),
        s(2, 5),
        s(3, 6)
      )))
    val withRow3 = withCol1 addRow nv(7, 8)
    assert(withRow3
      === matrNum(s(
        s(1, 4),
        s(2, 5),
        s(3, 6),
        s(7, 8)
      )))
  }
}
