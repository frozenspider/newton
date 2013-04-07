package org.newtonpolyhedron.solve.matrixminorgcd
import org.junit.runner.RunWith
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatrizMinorGCDSolverImplTest extends FunSuite {

  val solver = new MatrixMinorGCDSolverImpl

  test("3x3 case 1") {
    val matrix = matrFrac(a(
      a(1, 3, 4),
      a(3, 4, 2),
      a(0, 0, 0)))
    val expectedGcd = 5
    val expectedMinors = Seq(-10, -10, -5) map BigInt.apply

    val res = solver.lastRowGcd(matrix)
    assert(res._1 === expectedGcd)
    assert(res._2 === expectedMinors)
  }

  test("3x3 case 2") {
    val matrix = matrFrac(a(
      a(-3, -2, 1),
      a(5, -2, 1),
      a(0, 0, 0)))
    val expectedGcd = 8
    val expectedMinors = Seq(0, -8, 16) map BigInt.apply

    val res = solver.lastRowGcd(matrix)
    assert(res._1 === expectedGcd)
    assert(res._2 === expectedMinors)
  }

  test("4x4") {
    val matrix = matrFrac(a(
      a(1, 3, 5, 1),
      a(1, 2, -6, 9),
      a(2, 4, 3, 2),
      a(0, 0, 0, 0)))
    val expectedGcd = 1
    val expectedMinors = Seq(73, 56, 16, -15) map BigInt.apply

    val res = solver.lastRowGcd(matrix)
    assert(res._1 === expectedGcd)
    assert(res._2 === expectedMinors)
  }
}