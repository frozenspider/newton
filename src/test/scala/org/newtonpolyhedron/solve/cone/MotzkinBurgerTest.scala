package org.newtonpolyhedron.solve.cone

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.utils.NullPrintWriter

@RunWith(classOf[JUnitRunner])
class MotzkinBurgerTest extends FunSuite {

  val coneSolver = new MotzkinBurger

  implicit def tupleToVec(tuple: Tuple2[Int, Int]) =
    IntVec(tuple._1, tuple._2)

  implicit def tupleToVec(tuple: Tuple3[Int, Int, Int]) =
    IntVec(tuple._1, tuple._2, tuple._3)

  implicit def tupleToVec(tuple: Tuple4[Int, Int, Int, Int]) =
    IntVec(tuple._1, tuple._2, tuple._3, tuple._4)

  implicit def tupleToVec(tuple: Tuple5[Int, Int, Int, Int, Int]) =
    IntVec(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5)

  test("chernyakov") {
    def eqSys = IndexedSeq(
      (1, -1, 3, -8),
      (-1, 2, -1, 1),
      (2, -1, -2, 1),
      (-3, 1, -1, 6),
      (1, 1, -3, 2)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 4)
    assert(solution.toSet === (IndexedSeq(
      (15, 11, 12, 5),
      (13, 9, 12, 7),
      (5, 5, 8, 3),
      (1, -1, 2, 1)
    ) map tupleToVec).toSet)
  }

  test("chernyakov degenerated 4d") {
    def eqSys = IndexedSeq(
      (1, -1, 3, 0),
      (-1, 2, -1, 0),
      (2, -1, -2, 0),
      (-3, 1, -1, 0),
      (1, 1, -3, 0)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 4)
    assert(solution.toSet === (IndexedSeq(
      (0, 0, 0, 1),
      (0, 0, 0, -1)
    ) map tupleToVec).toSet)
  }

  test("chernyakov 5d (degenerated), with added zero") {
    def eqSys = IndexedSeq(
      (1, -1, 3, -8, 0),
      (-1, 2, -1, 1, 0),
      (2, -1, -2, 1, 0),
      (-3, 1, -1, 6, 0),
      (1, 1, -3, 2, 0)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 5)
    assert(solution.toSet === (IndexedSeq(
      (0, 0, 0, 0, 1),
      (0, 0, 0, 0, -1)
    ) map tupleToVec).toSet)
  }

  test("chernyakov 5d, with added number and negative indentity vectors") {
    def eqSys = IndexedSeq(
      (-1, 0, 0, 0, 0),
      (0, -1, 0, 0, 0),
      (0, 0, -1, 0, 0),
      (0, 0, 0, -1, 0),
      (0, 0, 0, 0, -1),
      (1, -1, 3, -8, 5),
      (-1, 2, -1, 1, -1),
      (2, -1, -2, 1, 0),
      (-3, 1, -1, 6, -3),
      (1, 1, -3, 2, -1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 5)
    assert(solution.toSet === (IndexedSeq(
      (15, 11, 12, 5, 0),
      (13, 0, 17, 8, 0),
      (5, 5, 8, 3, 0),
      (1, 1, 1, 1, 1),
      (13, 9, 12, 7, 0),
      (11, 0, 15, 8, 0),
      (2, 0, 3, 2, 1),
      (5, 0, 9, 4, 0)
    ) map tupleToVec).toSet)
  }

  test("chernyakov degenerated 5d 2") {
    def eqSys = IndexedSeq(
      (1, -1, 3, -8, 5),
      (-1, 2, -1, 1, -1),
      (2, -1, -2, 1, 0),
      (-3, 1, -1, 6, -3),
      (1, 1, -3, 2, -1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 5)
    assert(solution.toSet === (IndexedSeq(
      (1, 1, 1, 1, 1),
      (-1, -1, -1, -1, -1)
    ) map tupleToVec).toSet)
  }

  test("chernyakov 5d octant") {
    def eqSys = IndexedSeq(
      (-1, 0, 0, 0, 0),
      (0, -1, 0, 0, 0),
      (0, 0, -1, 0, 0),
      (0, 0, 0, -1, 0),
      (0, 0, 0, 0, -1),
      (1, -1, 3, -8, 5),
      (-1, 2, -1, 1, -1),
      (2, -1, -2, 1, 0),
      (-3, 1, -1, 6, -3),
      (1, 1, -3, 2, -1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 5)
    assert(solution.toSet === (IndexedSeq(
      (13, 0, 17, 8, 0),
      (15, 11, 12, 5, 0),
      (5, 0, 9, 4, 0),
      (2, 0, 3, 2, 1),
      (11, 0, 15, 8, 0),
      (5, 5, 8, 3, 0),
      (13, 9, 12, 7, 0),
      (1, 1, 1, 1, 1)
    ) map tupleToVec).toSet)
  }

  //
  //
  //

  test("bruno 3d test case") {
    def eqSys = IndexedSeq(
      (3, -1, -1),
      (-1, 3, -1),
      (-1, -1, 3),
      (1, -1, 1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 3)
    assert(solution.toSet === (IndexedSeq(
      (-1, -2, -1),
      (-1, -1, -2),
      (-2, -1, -1)
    ) map tupleToVec).toSet)
  }

  test("bruno 3d test case 2") {
    def eqSys = IndexedSeq(
      (3, -1, -1),
      (-1, 3, -1),
      (3, -1, 2),
      (-1, 3, 2)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 3)
    assert(solution.toSet === (IndexedSeq(
      (-1, -1, -2),
      (-1, -3, 0),
      (-3, -1, 0),
      (-1, -1, 1)
    ) map tupleToVec).toSet)
  }

  //
  //
  //

  test("bruno 4d, q4") {
    def eqSys = IndexedSeq(
      (-5, -1, -1, -1),
      (-4, -2, -1, -1),
      (-4, 0, -2, -2),
      (-4, 0, 0, 0),
      (-4, 4, 0, 0),
      (-4, 0, 4, 0),
      (-4, 0, 0, 4),
      (-8, 0, 0, 0),
      (-4, -4, 0, 0),
      (-4, 0, -4, 0),
      (-4, 0, 0, -4)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 4)
    assert(solution.toSet === (IndexedSeq(
      (1, -1, -1, -1),
      (1, 1, -1, -1),
      (1, 1, 1, 1),
      (1, -1, 1, 1),
      (1, 1, -1, 1),
      (1, -1, -1, 1),
      (1, 1, 1, -1),
      (1, -1, 1, -1)
    ) map tupleToVec).toSet)
  }

  test("bruno 4d, page 9") {
    def eqSys = IndexedSeq(
      (3, -1, -1, -1),
      (4, -2, -1, -1),
      (4, 0, -2, -2),
      (4, 0, 0, 0),
      (8, 0, 0, 0),
      (4, 4, 0, 0),
      (4, 0, 4, 0),
      (4, 0, 0, 4),
      (4, -4, 0, 0),
      (4, 0, -4, 0),
      (4, 0, 0, -4)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 4)
    assert(solution.toSet === (IndexedSeq(
      (-1, -1, 1, 1),
      (-1, 1, -1, 1),
      (-1, 1, 1, -1),
      (-1, -1, -1, -1),
      (-1, -1, -1, 1),
      (-1, 1, 1, 1),
      (-1, 1, -1, -1),
      (-1, -1, 1, -1)
    ) map tupleToVec).toSet)
  }

  test("bruno 4d, problematic case") {
    def eqSys = IndexedSeq(
      (2, 0, 0, 0),
      (2, 1, 0, 0),
      (2, 1, 1, 1),
      (2, 2, 1, 1),
      (2, 1, 2, 1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 4)
    assert(solution.toSet === (IndexedSeq(
      (0, 0, -1, 1),
      (0, 0, 1, -2),
      (0, -1, 0, 1),
      (-1, 2, 2, -4),
      (-1, 0, 0, 2)
    ) map tupleToVec).toSet)
  }

  //
  //
  //

  test("solodovnikov 4d, artificial") {
    def eqSys = IndexedSeq(
      (3, 4, -5, 6),
      (-2, -3, 3, -1),
      (1, 0, 0, 0),
      (0, 1, 0, 0),
      (0, 0, 1, 0),
      (0, 0, 0, 1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 4)
    assert(solution.toSet === (IndexedSeq(
      (0, -13, -14, -3),
      (-13, 0, -9, -1),
      (0, 0, -6, -5),
      (0, 0, -1, -3)
    ) map tupleToVec).toSet)
  }

  //
  //
  //

  test("simple 2d") {
    def eqSys = IndexedSeq(
      (1, -4),
      (-2, -1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 2)
    assert(solution.toSet === (IndexedSeq(
      (4, 1),
      (-1, 2)
    ) map tupleToVec).toSet)
  }

  test("simple 2d - 2") {
    def eqSys = IndexedSeq(
      (-1, 2),
      (2, -1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 2)
    assert(solution.toSet === (IndexedSeq(
      (-2, -1),
      (-1, -2)
    ) map tupleToVec).toSet)
  }

  test("simple 2d - 3") {
    def eqSys = IndexedSeq(
      (-1, 2),
      (3, -3)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 2)
    assert(solution.toSet === (IndexedSeq(
      (-2, -1),
      (-1, -1)
    ) map tupleToVec).toSet)
  }

  test("simple 2d - 4") {
    def eqSys = IndexedSeq(
      (-2, 1),
      (-3, 3)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 2)
    assert(solution.toSet === (IndexedSeq(
      (-1, -2),
      (1, 1)
    ) map tupleToVec).toSet)
  }

  //
  //
  //

  test("simple 3d") {
    def eqSys = IndexedSeq(
      (-3, 1, 1),
      (-1, 1, 0),
      (-1, 0, 1)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 3)
    assert(solution.toSet === (IndexedSeq(
      (-1, -1, -2),
      (-1, -2, -1),
      (1, 1, 1)
    ) map tupleToVec).toSet)
  }

  //
  //
  //

  test("random case") {
    def eqSys = IndexedSeq(
      (-9, 8, 0),
      (-9, 0, 7),
      (-6, 2, 1),
      (-3, 4, 0),
      (-3, 0, 5),
      (-2, 2, 2)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 3)
    assert(solution.toSet === (IndexedSeq(
      (-8, -9, -30),
      (-14, -33, -18),
      (0, 0, -1),
      (0, -1, 0),
      (4, 3, 1),
      (5, 2, 3)
    ) map tupleToVec).toSet)
  }

  test("random case 2") {
    def eqSys = IndexedSeq(
      (9, -8, 0),
      (0, -8, 7),
      (3, -6, 1),
      (3, -4, 0),
      (0, -4, 5),
      (1, -2, 2)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 3)
    assert(solution.toSet === (IndexedSeq(
      (-34, -21, -24),
      (0, 0, -1),
      (-4, -3, -6),
      (-1, 0, 0),
      (8, 9, 5),
      (2, 5, 4)
    ) map tupleToVec).toSet)
  }

  test("random case 3") {
    def eqSys = IndexedSeq(
      (9, 0, -7),
      (0, 8, -7),
      (3, 2, -6),
      (3, 0, -5),
      (0, 4, -5),
      (1, 2, -3)
    ) map tupleToVec
    val solution = coneSolver.solve(eqSys, None, 3)
    assert(solution.toSet === (IndexedSeq(
      (56, 63, 72),
      (0, -1, 0),
      (-1, 0, 0),
      (-20, -15, -12)
    ) map tupleToVec).toSet)
  }

  test("1d case") {
    // Not even sure if this one is right
    def eqSys = IndexedSeq(IntVec(3))
    val solution = coneSolver.solve(eqSys, None, 1)
    assert(solution === IndexedSeq(IntVec(-1)))
  }
}
