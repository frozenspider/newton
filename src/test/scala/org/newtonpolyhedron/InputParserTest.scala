package org.newtonpolyhedron

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.utils.parsing.ParseFormats._

@RunWith(classOf[JUnitRunner])
class InputParserTest
    extends FunSuite
    with InternalMathProcessorMixin {

  def toLines(s: String) = InputParser.refineLines(s.lines.toStream)

  //
  // Parse vectors
  //

  test("parse vectors - integer") {
    val str =
      """1 2 3
         4 5 6"""
    val section = InputParser.parseVectors(3)(toLines(str))(parseInt)
    assert(section === s(iv(1, 2, 3), iv(4, 5, 6)))
  }

  test("parse vectors - fraction") {
    val str =
      """1 2 3
         4 5 6"""
    val section = InputParser.parseVectors(3)(toLines(str))(parseNum)
    assert(section === s(nv(1, 2, 3), nv(4, 5, 6)))
  }

  test("parse vectors - complex fraction") {
    val str =
      """1.0 2.00 009
         2.22 3/2 6/2"""
    val section = InputParser.parseVectors(3)(toLines(str))(parseNum)
    assert(section === s(nv(1, 2, 9), nv2(frac(222, 100), frac(3, 2), frac(6, 2))))
  }

  //
  // Parse single poly
  //

  test("parse poly - empty") {
    val str =
      """
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(toLines(str))(parseNum)
    assert(pointList === s())
    assert(commonLimits === None)
    assert(basis === None)
  }

  test("parse poly - simplest") {
    val str =
      """
      3
      1 2 3
      4 5 6
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(toLines(str))(parseNum)
    assert(pointList === s(nv(1, 2, 3), nv(4, 5, 6)))
    assert(commonLimits === None)
    assert(basis === None)
  }

  test("parse poly - basis") {
    val str =
      """
      3
      #
      1 0 0
      0 0 1
      3 1 -1
      2 3 4
      #
      1 2 3
      4 5 6
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(toLines(str))(parseNum)
    assert(pointList === s(nv(1, 2, 3), nv(4, 5, 6)))
    assert(commonLimits === None)
    assert(basis === Some(s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)
    )))
  }

  test("parse poly - limits") {
    val str = //
      """
      3
      $
      1 0 0
      0 0 1
      3 1 -1
      2 3 4
      $
      1 2 3
      4 5 6
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(toLines(str))(parseNum)
    assert(pointList === s(nv(1, 2, 3), nv(4, 5, 6)))
    assert(commonLimits === Some(s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)
    )))
    assert(basis === None)
  }

  test("parse poly - both") {
    val str =
      """
      3
      $
      1 0 0
      0 0 1
      3 1 -1
      2 3  4
      $
       #
      0     2 3
       1 9    2
        #
      1   2.00 3
        4  10/2 6

      @

      comment

      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(toLines(str))(parseNum)
    assert(pointList === s(nv(1, 2, 3), nv(4, 5, 6)))
    assert(commonLimits === Some(s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)
    )))
    assert(basis === Some(s(
      iv(0, 2, 3),
      iv(1, 9, 2)
    )))
  }

  //
  // Parse multiple polys
  //

  test("parse polys - two") {
    val str =
      """
      3
      9 0 0
      0 8 0
      0 0 7
      3 2 1
      %
      3 0 0
      0 4 0
      0 0 5
      1 2 2
      """
    val (polys, dim) = InputParser.parsePolysFromLines(toLines(str))(parseNum)
    assert(dim === 3)
    assert(polys.size === 2)
    assert(polys === s(
      s(
        nv(9, 0, 0),
        nv(0, 8, 0),
        nv(0, 0, 7),
        nv(3, 2, 1)
      ),
      s(
        nv(3, 0, 0),
        nv(0, 4, 0),
        nv(0, 0, 5),
        nv(1, 2, 2)
      )
    ))
  }

  test("parse polys - three") {
    val str =
      """
      4
      9 0 0 1
      0 8 0 2
      0 0 7 3
      3 2 1 4
      %
      3 0 0 4
      0 4 0 3
      0 0 5 2
      1 2 2 1
      %
      1 2 2 4
      0 1 2 3
      0 0 1 2
      4 3 2 1
      5 6 7 8
      """
    val (polys, dim) = InputParser.parsePolysFromLines(toLines(str))(parseNum)
    assert(dim === 4)
    assert(polys.size === 3)
    assert(polys === s(
      s(
        nv(9, 0, 0, 1),
        nv(0, 8, 0, 2),
        nv(0, 0, 7, 3),
        nv(3, 2, 1, 4)
      ),
      s(
        nv(3, 0, 0, 4),
        nv(0, 4, 0, 3),
        nv(0, 0, 5, 2),
        nv(1, 2, 2, 1)
      ),
      s(
        nv(1, 2, 2, 4),
        nv(0, 1, 2, 3),
        nv(0, 0, 1, 2),
        nv(4, 3, 2, 1),
        nv(5, 6, 7, 8)
      )
    ))
  }

  //
  // Parse matrix
  //

  test("parse matrix - fractions 1") {
    val str =
      """
      4
      9 0 0 1
      0 8 0 2
      0 0 7 3
      3 2 1 4
      """
    val matrix = InputParser.parseMatrixFromLines(toLines(str))(Matrix.apply[Rational], parseFrac).get
    assert(matrix.isSquare === true)
    assert(matrix.rowCount === 4)
    assert(matrix === matrFrac(
      s(
        s(9, 0, 0, 1),
        s(0, 8, 0, 2),
        s(0, 0, 7, 3),
        s(3, 2, 1, 4)
      )
    ))
  }

  test("parse matrix - ints") {
    val str =
      """
      3
      9 0 0
      0 8 0
      0 0 7
      """
    val matrix = InputParser.parseMatrixFromLines(toLines(str))(Matrix.apply[BigInt], parseInt).get
    assert(matrix.isSquare === true)
    assert(matrix.rowCount === 3)
    assert(matrix.colCount === 3)
    assert(matrix === matrInt(
      s(
        s(9, 0, 0),
        s(0, 8, 0),
        s(0, 0, 7)
      )
    ))
  }

  test("parse matrix while skipping rows - ints") {
    val str =
      """
      3
      1 2
      9 0 0
      0 8 0
      0 0 7
      @
      Comment!
      """
    val (matrix, r, c) = InputParser.parseMatrixWithSkipFromLines(toLines(str))(Matrix.apply[BigInt], parseInt).get
    assert(r === 1)
    assert(c === 2)
    assert(matrix.isSquare === true)
    assert(matrix.rowCount === 3)
    assert(matrix.colCount === 3)
    assert(matrix === matrInt(
      s(
        s(9, 0, 0),
        s(0, 8, 0),
        s(0, 0, 7)
      )
    ))
  }

}
