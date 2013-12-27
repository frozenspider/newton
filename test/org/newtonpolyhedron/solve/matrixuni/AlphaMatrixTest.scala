package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.utils.StringUtils
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.BigFrac

@RunWith(classOf[JUnitRunner])
class AlphaMatrixTest extends FunSuite {

  val maker = new UnimodularMatrixMakerImpl

  private def matrixFromDraft(draft: Array[Array[Int]]): Matrix[BigFrac] = {
    matrFrac(draft :+ Array.fill(draft.size + 1)(0))
  }

  private def performTheTest(source: Array[Array[Int]], expected: Matrix[BigFrac]): Unit = {
    val alpha = maker.unimodularFrom(matrixFromDraft(source))
    println("=====")
    assert(alpha === expected)
  }

  //
  // Stable
  //
  test("3d, == ==") {
    performTheTest(a(
      a(2, 4, 5),
      a(3, 5, 1)
    ), matrFrac(a(
      a(2, 4, 5),
      a(3, 5, 1),
      a(1, 2, 2))))
  }

  test("3d, == ==  ~2") {
    performTheTest(a(
      a(-3, 5, -3),
      a(4, -7, 3)
    ), matrFrac(a(
      a(-3, 5, -3),
      a(4, -7, 3),
      a(0, 0, 1))))
  }

  test("3d, == ==  ~3") {
    performTheTest(a(
      a(1, 2, 3),
      a(-1, 1, 1)
    ), matrFrac(a(
      a(1, 2, 3),
      a(-1, 1, 1),
      a(0, -1, -1))))
  }

  test("4d, == == ==") {
    performTheTest(a(
      a(2, 0, 1, 1),
      a(-2, 1, 0, 1),
      a(-1, 1, 1, 1)
    ), matrFrac(a(
      a(2, 0, 1, 1),
      a(-2, 1, 0, 1),
      a(-1, 1, 1, 1),
      a(0, 0, 0, 1))))
  }

  //
  // Inversed
  //
  test("3d, -- --") {
    performTheTest(a(
      a(5, 8, 3),
      a(3, 5, 4)
    ), matrFrac(a(
      a(-5, -8, -3),
      a(-3, -5, -4),
      a(0, 0, 1))))
  }

  test("3d, -- --  ~2") {
    performTheTest(a(
      a(-3, -4, 1),
      a(4, 5, 2)
    ), matrFrac(a(
      a(3, 4, -1),
      a(-4, -5, -2),
      a(0, 0, 1))))
  }

  test("3d, -- --  ~3") {
    performTheTest(a(
      a(2, -3, 4),
      a(4, -2, 3)
    ), matrFrac(a(
      a(-2, 3, -4),
      a(-4, 2, -3),
      a(-3, 3, -4))))
  }

  test("3d, -- --  ~4") {
    performTheTest(a(
      a(-1, 4, 5),
      a(4, 5, 6)
    ), matrFrac(a(
      a(1, -4, -5),
      a(-4, -5, -6),
      a(0, -4, -5))))
  }

  test("3d, -- --  ~5") {
    performTheTest(a(
      a(7, -2, 8),
      a(-6, 1, -7)
    ), matrFrac(a(
      a(-7, 2, -8),
      a(6, -1, 7),
      a(-6, 2, -7))))
  }

  test("4d, -- -- --") {
    performTheTest(a(
      a(3, 2, 1, 2),
      a(1, 1, 1, 1),
      a(2, 2, 1, -5)
    ), matrFrac(a(
      a(-3, -2, -1, -2),
      a(-1, -1, -1, -1),
      a(-2, -2, -1, 5),
      a(0, 0, 0, 1))))
  }

  //
  // Rest
  //
  test("3d, -- <>") {
    performTheTest(a(
      a(3, 5, 4),
      a(5, 8, 3)
    ), matrFrac(a(
      a(-3, -5, -4),
      a(-7, -12, -13),
      a(0, 0, 1))))
  }

  test("3d, -- <>  ~2") {
    performTheTest(a(
      a(3, 5, 1),
      a(2, 4, 5)
    ), matrFrac(a(
      a(-3, -5, -1),
      a(2, 4, 5),
      a(1, 2, 2))))
  }

  test("3d, -- <>  ~3") {
    performTheTest(a(
      a(2, -3, 2),
      a(-3, 4, 1)
    ), matrFrac(a(
      a(-2, 3, -2),
      a(1, -2, 5),
      a(0, 0, 1))))
  }

  test("3d, == <>") {
    performTheTest(a(
      a(-3, 4, 1),
      a(-2, 3, 2)
    ), matrFrac(a(
      a(-3, 4, 1),
      a(-4, 5, 0),
      a(0, 0, 1))))
  }

  test("3d, == <>  ~2") {
    performTheTest(a(
      a(4, 5, 2),
      a(-3, -4, 1)
    ), matrFrac(a(
      a(4, 5, 2),
      a(-5, -6, -5),
      a(0, 0, 1))))
  }

  test("3d, -- <>  ~4") {
    performTheTest(a(
      a(4, -7, 3),
      a(-3, 5, -3)
    ), matrFrac(a(
      a(-4, 7, -3),
      a(5, -9, 3),
      a(0, 0, 1))))
  }

  test("3d, == <>  ~3") {
    performTheTest(a(
      a(4, 5, 6),
      a(-1, 4, 5)
    ), matrFrac(a(
      a(4, 5, 6),
      a(41, 46, 55),
      a(-5, -5, -6))))
  }

  test("3d, == <>  ~4") {
    performTheTest(a(
      a(-6, 1, -7),
      a(7, -2, 8)
    ), matrFrac(a(
      a(-6, 1, -7),
      a(17, -2, 20),
      a(1, 0, 1))))
  }

  test("3d, == <>  ~5") {
    performTheTest(a(
      a(1, -6, -7),
      a(-2, 7, 8)
    ), matrFrac(a(
      a(1, -6, -7),
      a(-2, 17, 20),
      a(0, -1, -1))))
  }

  test("3d, == <>  ~6") {
    performTheTest(a(
      a(-7, 1, -6),
      a(8, -2, 7)
    ), matrFrac(a(
      a(-7, 1, -6),
      a(20, -2, 17),
      a(-1, 0, -1))))
  }

  test("3d, == <>  ~7 [2013-12-21 email]") {
    performTheTest(a(
      a(1, 3, 4),
      a(-4, 1, 1)
    ), matrFrac(a(
      a(1, 3, 4),
      a(-4, -25, -33),
      a(0, -3, -4))))
  }
  
  test("3d, == <>  ~7 [2013-12-21 email, ex2, part of full example]") {
    performTheTest(a(
      a(3, 4, 2),
      a(2, 1, 3)
    ), matrFrac(a(
      a(3, 4, 2),
      a(-4, -5, -3),
      a(0, 0, 1))))
  }

  test("4d, == <> <>") {
    performTheTest(a(
      a(1, 1, 1, 1),
      a(2, 2, 1, -5),
      a(3, 2, 1, 2)
    ), matrFrac(a(
      a(1, 1, 1, 1),
      a(2, 2, 3, 9),
      a(3, 2, 5, 30),
      a(0, 0, 0, 1))))
  }

  test("4d, == <> <>  ~2") {
    performTheTest(a(
      a(2, 2, 1, -5),
      a(1, 1, 1, 1),
      a(3, 2, 1, 2)
    ), matrFrac(a(
      a(2, 2, 1, -5),
      a(3, 3, 1, -11),
      a(1, 2, 1, -12),
      a(0, 0, 0, 1))))
  }

  test("4d, -- == <>") {
    performTheTest(a(
      a(3, 2, 1, 2),
      a(2, 2, 1, -5),
      a(1, 1, 1, 1)
    ), matrFrac(a(
      a(-3, -2, -1, -2),
      a(2, 2, 1, -5),
      a(3, 3, 1, -11),
      a(0, 0, 0, 1))))
  }

  test("4d, == <> <>  ~3") {
    performTheTest(a(
      a(2, 2, 1, -5),
      a(3, 2, 1, 2),
      a(1, 1, 1, 1)
    ), matrFrac(a(
      a(2, 2, 1, -5),
      a(1, 2, 1, -12),
      a(3, 1, 1, 15),
      a(0, 0, 0, 1))))
  }

  test("4d, == <> <>  ~4") {
    performTheTest(a(
      a(1, 2, 3, 1),
      a(2, 2, 1, -5),
      a(3, 2, 1, 2)
    ), matrFrac(a(
      a(1, 2, 3, 1),
      a(2, 6, 11, 9),
      a(3, 10, 17, 4),
      a(0, 1, 2, 0))))
  }

  test("3d, -- == <>") {
    performTheTest(a(
      a(3, 2, 1, 2),
      a(2, 2, 1, -5),
      a(1, 2, 3, 1)
    ), matrFrac(a(
      a(-3, -2, -1, -2),
      a(2, 2, 1, -5),
      a(7, 6, 5, 5),
      a(1, 1, 0, -6))))
  }

  test("3d, == <> <>  ~5") {
    performTheTest(a(
      a(1, 2, 3, 1),
      a(3, 2, 1, 2),
      a(2, 2, 1, -5)
    ), matrFrac(a(
      a(1, 2, 3, 1),
      a(3, 10, 17, 4),
      a(2, 58, 113, 9),
      a(0, 1, 2, 0))))
  }

  test("4d, == <> <>  ~6") {
    performTheTest(a(
      a(2, 2, 1, -5),
      a(3, 2, 1, 2),
      a(1, 2, 3, 1)
    ), matrFrac(a(
      a(2, 2, 1, -5),
      a(1, 2, 1, -12),
      a(11, 10, 3, -31),
      a(0, 1, 0, -13))))
  }

  test("3d, -- <> ==") {
    performTheTest(a(
      a(3, 2, 1, 2),
      a(1, 2, 3, 1),
      a(2, 2, 1, -5)
    ), matrFrac(a(
      a(-3, -2, -1, -2),
      a(7, 6, 5, 5),
      a(2, 2, 1, -5),
      a(1, 1, 1, 1))))
  }

  test("4d, -- <> <>") {
    performTheTest(a(
      a(-1, 1, 1, 1),
      a(2, 0, 1, 1),
      a(-2, 1, 0, 1)
    ), matrFrac(a(
      a(1, -1, -1, -1),
      a(-2, 4, 5, 5),
      a(2, -5, -6, -7),
      a(0, 0, 0, 1))))
  }

  test("4d, == == <>  ~2") {
    performTheTest(a(
      a(2, 0, 1, 1),
      a(-1, 1, 1, 1),
      a(-2, 1, 0, 1)
    ), matrFrac(a(
      a(2, 0, 1, 1),
      a(-1, 1, 1, 1),
      a(-4, 1, 0, -1),
      a(0, 0, 0, 1))))
  }

  test("4d, == == <>") {
    performTheTest(a(
      a(2, 2, 1, -5),
      a(1, 2, 3, 1),
      a(3, 2, 1, 2)
    ), matrFrac(a(
      a(2, 2, 1, -5),
      a(1, 2, 3, 1),
      a(11, 10, 1, -44),
      a(1, 1, 0, -6))))
  }

  test("4d, == <> <>  ~5") {
    performTheTest(a(
      a(1, 2, 3, 1),
      a(1, 1, 1, 1),
      a(3, 2, 1, 2)
    ), matrFrac(a(
      a(1, 2, 3, 1),
      a(1, 3, 5, 1),
      a(3, 10, 17, 4),
      a(0, 0, -1, 0))))
  }

}