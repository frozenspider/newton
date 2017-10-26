package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.utils.StringUtils
import spire.math.Rational

@RunWith(classOf[JUnitRunner])
class AlphaMatrixTest
    extends FunSuite
    with InternalMathProcessorMixin {

  val maker = new UnimodularMatrixMakerImpl

  private def matrixFromDraft(draft: Seq[Seq[Int]]): M = {
    matrNum(draft :+ Seq.fill(draft.size + 1)(0))
  }

  private def performTheTest(source: Seq[Seq[Int]], expected: M): Unit = {
    val alpha = maker.unimodularFrom(matrixFromDraft(source))
    assert(alpha === expected)
  }

  //
  // Stable
  //

  test("3d, == ==") {
    performTheTest(s(
      s(2, 4, 5),
      s(3, 5, 1)
    ), matrNum(s(
      s(2, 4, 5),
      s(3, 5, 1),
      s(1, 2, 2)
    )))
  }

  test("3d, == ==  ~2") {
    performTheTest(s(
      s(-3, 5, -3),
      s(4, -7, 3)
    ), matrNum(s(
      s(-3, 5, -3),
      s(4, -7, 3),
      s(0, 0, 1)
    )))
  }

  test("3d, == ==  ~3") {
    performTheTest(s(
      s(1, 2, 3),
      s(-1, 1, 1)
    ), matrNum(s(
      s(1, 2, 3),
      s(-1, 1, 1),
      s(0, -1, -1)
    )))
  }

  test("4d, == == ==") {
    performTheTest(s(
      s(2, 0, 1, 1),
      s(-2, 1, 0, 1),
      s(-1, 1, 1, 1)
    ), matrNum(s(
      s(2, 0, 1, 1),
      s(-2, 1, 0, 1),
      s(-1, 1, 1, 1),
      s(0, 0, 0, 1)
    )))
  }

  //
  // Inversed
  //

  test("3d, -- --") {
    performTheTest(s(
      s(5, 8, 3),
      s(3, 5, 4)
    ), matrNum(s(
      s(-5, -8, -3),
      s(-3, -5, -4),
      s(0, 0, 1)
    )))
  }

  test("3d, -- --  ~2") {
    performTheTest(s(
      s(-3, -4, 1),
      s(4, 5, 2)
    ), matrNum(s(
      s(3, 4, -1),
      s(-4, -5, -2),
      s(0, 0, 1)
    )))
  }

  test("3d, -- --  ~3") {
    performTheTest(s(
      s(2, -3, 4),
      s(4, -2, 3)
    ), matrNum(s(
      s(-2, 3, -4),
      s(-4, 2, -3),
      s(-3, 3, -4)
    )))
  }

  test("3d, -- --  ~4") {
    performTheTest(s(
      s(-1, 4, 5),
      s(4, 5, 6)
    ), matrNum(s(
      s(1, -4, -5),
      s(-4, -5, -6),
      s(0, -4, -5)
    )))
  }

  test("3d, -- --  ~5") {
    performTheTest(s(
      s(7, -2, 8),
      s(-6, 1, -7)
    ), matrNum(s(
      s(-7, 2, -8),
      s(6, -1, 7),
      s(-6, 2, -7)
    )))
  }

  test("4d, -- -- --") {
    performTheTest(s(
      s(3, 2, 1, 2),
      s(1, 1, 1, 1),
      s(2, 2, 1, -5)
    ), matrNum(s(
      s(-3, -2, -1, -2),
      s(-1, -1, -1, -1),
      s(-2, -2, -1, 5),
      s(0, 0, 0, 1)
    )))
  }

  //
  // Rest
  //

  test("3d, -- <>") {
    performTheTest(s(
      s(3, 5, 4),
      s(5, 8, 3)
    ), matrNum(s(
      s(-3, -5, -4),
      s(-7, -12, -13),
      s(0, 0, 1)
    )))
  }

  test("3d, -- <>  ~2") {
    performTheTest(s(
      s(3, 5, 1),
      s(2, 4, 5)
    ), matrNum(s(
      s(-3, -5, -1),
      s(2, 4, 5),
      s(1, 2, 2)
    )))
  }

  test("3d, -- <>  ~3") {
    performTheTest(s(
      s(2, -3, 2),
      s(-3, 4, 1)
    ), matrNum(s(
      s(-2, 3, -2),
      s(1, -2, 5),
      s(0, 0, 1)
    )))
  }

  test("3d, == <>") {
    performTheTest(s(
      s(-3, 4, 1),
      s(-2, 3, 2)
    ), matrNum(s(
      s(-3, 4, 1),
      s(-4, 5, 0),
      s(0, 0, 1)
    )))
  }

  test("3d, == <>  ~2") {
    performTheTest(s(
      s(4, 5, 2),
      s(-3, -4, 1)
    ), matrNum(s(
      s(4, 5, 2),
      s(-5, -6, -5),
      s(0, 0, 1)
    )))
  }

  test("3d, -- <>  ~4") {
    performTheTest(s(
      s(4, -7, 3),
      s(-3, 5, -3)
    ), matrNum(s(
      s(-4, 7, -3),
      s(5, -9, 3),
      s(0, 0, 1)
    )))
  }

  test("3d, == <>  ~3") {
    performTheTest(s(
      s(4, 5, 6),
      s(-1, 4, 5)
    ), matrNum(s(
      s(4, 5, 6),
      s(41, 46, 55),
      s(-5, -5, -6)
    )))
  }

  test("3d, == <>  ~4") {
    performTheTest(s(
      s(-6, 1, -7),
      s(7, -2, 8)
    ), matrNum(s(
      s(-6, 1, -7),
      s(17, -2, 20),
      s(1, 0, 1)
    )))
  }

  test("3d, == <>  ~5") {
    performTheTest(s(
      s(1, -6, -7),
      s(-2, 7, 8)
    ), matrNum(s(
      s(1, -6, -7),
      s(-2, 17, 20),
      s(0, -1, -1)
    )))
  }

  test("3d, == <>  ~6") {
    performTheTest(s(
      s(-7, 1, -6),
      s(8, -2, 7)
    ), matrNum(s(
      s(-7, 1, -6),
      s(20, -2, 17),
      s(-1, 0, -1)
    )))
  }

  test("3d, == <>  ~7 [2013-12-21 email]") {
    performTheTest(s(
      s(1, 3, 4),
      s(-4, 1, 1)
    ), matrNum(s(
      s(1, 3, 4),
      s(-4, -25, -33),
      s(0, -3, -4)
    )))
  }

  test("3d, == <>  ~7 [2013-12-21 email, ex2, part of full example]") {
    performTheTest(s(
      s(3, 4, 2),
      s(2, 1, 3)
    ), matrNum(s(
      s(3, 4, 2),
      s(-4, -5, -3),
      s(0, 0, 1)
    )))
  }

  test("4d, == <> <>") {
    performTheTest(s(
      s(1, 1, 1, 1),
      s(2, 2, 1, -5),
      s(3, 2, 1, 2)
    ), matrNum(s(
      s(1, 1, 1, 1),
      s(2, 2, 3, 9),
      s(3, 2, 5, 30),
      s(0, 0, 0, 1)
    )))
  }

  test("4d, == <> <>  ~2") {
    performTheTest(s(
      s(2, 2, 1, -5),
      s(1, 1, 1, 1),
      s(3, 2, 1, 2)
    ), matrNum(s(
      s(2, 2, 1, -5),
      s(3, 3, 1, -11),
      s(1, 2, 1, -12),
      s(0, 0, 0, 1)
    )))
  }

  test("4d, -- == <>") {
    performTheTest(s(
      s(3, 2, 1, 2),
      s(2, 2, 1, -5),
      s(1, 1, 1, 1)
    ), matrNum(s(
      s(-3, -2, -1, -2),
      s(2, 2, 1, -5),
      s(3, 3, 1, -11),
      s(0, 0, 0, 1)
    )))
  }

  test("4d, == <> <>  ~3") {
    performTheTest(s(
      s(2, 2, 1, -5),
      s(3, 2, 1, 2),
      s(1, 1, 1, 1)
    ), matrNum(s(
      s(2, 2, 1, -5),
      s(1, 2, 1, -12),
      s(3, 1, 1, 15),
      s(0, 0, 0, 1)
    )))
  }

  test("4d, == <> <>  ~4") {
    performTheTest(s(
      s(1, 2, 3, 1),
      s(2, 2, 1, -5),
      s(3, 2, 1, 2)
    ), matrNum(s(
      s(1, 2, 3, 1),
      s(2, 6, 11, 9),
      s(3, 10, 17, 4),
      s(0, 1, 2, 0)
    )))
  }

  test("3d, -- == <>") {
    performTheTest(s(
      s(3, 2, 1, 2),
      s(2, 2, 1, -5),
      s(1, 2, 3, 1)
    ), matrNum(s(
      s(-3, -2, -1, -2),
      s(2, 2, 1, -5),
      s(7, 6, 5, 5),
      s(1, 1, 0, -6)
    )))
  }

  test("3d, == <> <>  ~5") {
    performTheTest(s(
      s(1, 2, 3, 1),
      s(3, 2, 1, 2),
      s(2, 2, 1, -5)
    ), matrNum(s(
      s(1, 2, 3, 1),
      s(3, 10, 17, 4),
      s(2, 58, 113, 9),
      s(0, 1, 2, 0)
    )))
  }

  test("4d, == <> <>  ~6") {
    performTheTest(s(
      s(2, 2, 1, -5),
      s(3, 2, 1, 2),
      s(1, 2, 3, 1)
    ), matrNum(s(
      s(2, 2, 1, -5),
      s(1, 2, 1, -12),
      s(11, 10, 3, -31),
      s(0, 1, 0, -13)
    )))
  }

  test("3d, -- <> ==") {
    performTheTest(s(
      s(3, 2, 1, 2),
      s(1, 2, 3, 1),
      s(2, 2, 1, -5)
    ), matrNum(s(
      s(-3, -2, -1, -2),
      s(7, 6, 5, 5),
      s(2, 2, 1, -5),
      s(1, 1, 1, 1)
    )))
  }

  test("4d, -- <> <>") {
    performTheTest(s(
      s(-1, 1, 1, 1),
      s(2, 0, 1, 1),
      s(-2, 1, 0, 1)
    ), matrNum(s(
      s(1, -1, -1, -1),
      s(-2, 4, 5, 5),
      s(2, -5, -6, -7),
      s(0, 0, 0, 1)
    )))
  }

  test("4d, == == <>  ~2") {
    performTheTest(s(
      s(2, 0, 1, 1),
      s(-1, 1, 1, 1),
      s(-2, 1, 0, 1)
    ), matrNum(s(
      s(2, 0, 1, 1),
      s(-1, 1, 1, 1),
      s(-4, 1, 0, -1),
      s(0, 0, 0, 1)
    )))
  }

  test("4d, == == <>") {
    performTheTest(s(
      s(2, 2, 1, -5),
      s(1, 2, 3, 1),
      s(3, 2, 1, 2)
    ), matrNum(s(
      s(2, 2, 1, -5),
      s(1, 2, 3, 1),
      s(11, 10, 1, -44),
      s(1, 1, 0, -6)
    )))
  }

  test("4d, == <> <>  ~5") {
    performTheTest(s(
      s(1, 2, 3, 1),
      s(1, 1, 1, 1),
      s(3, 2, 1, 2)
    ), matrNum(s(
      s(1, 2, 3, 1),
      s(1, 3, 5, 1),
      s(3, 10, 17, 4),
      s(0, 0, -1, 0)
    )))
  }

}
