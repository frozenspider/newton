package org.newtonpolyhedron

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.vector.FracMathVec.FracMathVecFormat
import org.newtonpolyhedron.entity.vector.IntMathVec.IntMathVecFormat
import org.newtonpolyhedron.entity.MatrixSupport
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.BigFrac

@RunWith(classOf[JUnitRunner])
class InputParserTest extends FunSuite {

  def toLines(s: String) = InputParser.refineLines(s.lines.toStream)

  //
  // Parse vectors
  //
  test("parse vectors - integer") {
    val str = // 
      """1 2 3
         4 5 6"""
    val section = InputParser.parseVectors(3)(IntMathVecFormat, toLines(str))
    assert(section === s(iv(1, 2, 3), iv(4, 5, 6)))
  }

  test("parse vectors - fraction") {
    val str = // 
      """1 2 3
         4 5 6"""
    val section = InputParser.parseVectors(3)(FracMathVecFormat, toLines(str))
    assert(section === s(fv(1, 2, 3), fv(4, 5, 6)))
  }

  test("parse vectors - complex fraction") {
    val str = // 
      """1.0 2.00 009
         2.22 3/2 6/2"""
    val section = InputParser.parseVectors(3)(FracMathVecFormat, toLines(str))
    assert(section === s(fv(1, 2, 9), fv2(bf(222, 100), bf(3, 2), bf(6, 2))))
  }

  //
  // Parse single poly
  //
  test("parse poly - empty") {
    val str = // 
      """  
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(FracMathVecFormat)(toLines(str))
    assert(pointList === s())
    assert(commonLimits === s())
    assert(basis === s())
  }

  test("parse poly - simplest") {
    val str = // 
      """  
      3
      1 2 3
      4 5 6
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(FracMathVecFormat)(toLines(str))
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s())
    assert(basis === s())
  }

  test("parse poly - basis") {
    val str = // 
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
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(FracMathVecFormat)(toLines(str))
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s())
    assert(basis === s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)))
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
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(FracMathVecFormat)(toLines(str))
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)))
    assert(basis === s())
  }

  test("parse poly - both") {
    val str = // 
      """
      3
      $         
      1 0 0       
      0 0 1       
      3 1 -1      
      2 3  4      
      $           
       #          
      0 		2 3           
       1 9    2   
      	#        
      1 	2.00 3   
        4  10/2 6     
      
      @
      
      comment
      
      """
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromLines(FracMathVecFormat)(toLines(str))
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)))
    assert(basis === s(
      iv(0, 2, 3),
      iv(1, 9, 2)))
  }

  //
  // Parse multiple polys
  //
  test("parse polys - two") {
    val str = // 
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
    val (polys, dim) = InputParser.parsePolysFromLines(FracMathVecFormat)(toLines(str))
    assert(dim === 3)
    assert(polys.size === 2)
    assert(polys === s(
      s(
        iv(9, 0, 0),
        iv(0, 8, 0),
        iv(0, 0, 7),
        iv(3, 2, 1)),
      s(
        iv(3, 0, 0),
        iv(0, 4, 0),
        iv(0, 0, 5),
        iv(1, 2, 2))))
  }

  test("parse polys - three") {
    val str = // 
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
    val (polys, dim) = InputParser.parsePolysFromLines(FracMathVecFormat)(toLines(str))
    assert(dim === 4)
    assert(polys.size === 3)
    assert(polys === s(
      s(
        iv(9, 0, 0, 1),
        iv(0, 8, 0, 2),
        iv(0, 0, 7, 3),
        iv(3, 2, 1, 4)),
      s(
        iv(3, 0, 0, 4),
        iv(0, 4, 0, 3),
        iv(0, 0, 5, 2),
        iv(1, 2, 2, 1)),
      s(
        iv(1, 2, 2, 4),
        iv(0, 1, 2, 3),
        iv(0, 0, 1, 2),
        iv(4, 3, 2, 1),
        iv(5, 6, 7, 8))))
  }

  //
  // Parse matrix
  //
  test("parse matrix - fractions 1") {
    val str = // 
      """
      4
      9 0 0 1
      0 8 0 2
      0 0 7 3
      3 2 1 4
      """
    val matrix = InputParser.parseMatrixFromLines(FracMathVecFormat, MatrixSupport.fromFracs)(toLines(str)).get
    assert(matrix.isSquare === true)
    assert(matrix.rowCount === 4)
    assert(matrix === matrFrac(
      a(
        a(9, 0, 0, 1),
        a(0, 8, 0, 2),
        a(0, 0, 7, 3),
        a(3, 2, 1, 4))))
  }

  test("parse matrix - ints") {
    val str = // 
      """
      3
      9 0 0
      0 8 0
      0 0 7
      """
    val matrix = InputParser.parseMatrixFromLines(IntMathVecFormat, MatrixSupport.fromInts)(toLines(str)).get
    assert(matrix.isSquare === true)
    assert(matrix.rowCount === 3)
    assert(matrix.colCount === 3)
    assert(matrix === matrInt(
      a(
        a(9, 0, 0),
        a(0, 8, 0),
        a(0, 0, 7))))
  }

  test("parse matrix while skipping rows - ints") {
    val str = // 
      """
      3
      1 2
      9 0 0
      0 8 0
      0 0 7
      @
      Comment!
      """
    val (matrix, r, c) = InputParser.parseMatrixWithSkipFromLines(IntMathVecFormat, MatrixSupport.fromInts)(toLines(str)).get
    assert(r === 1)
    assert(c === 2)
    assert(matrix.isSquare === true)
    assert(matrix.rowCount === 3)
    assert(matrix.colCount === 3)
    assert(matrix === matrInt(
      a(
        a(9, 0, 0),
        a(0, 8, 0),
        a(0, 0, 7))))
  }

}