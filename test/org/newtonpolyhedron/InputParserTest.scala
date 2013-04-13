package org.newtonpolyhedron

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.vector.FracMathVec.FracMathVecFormat
import org.newtonpolyhedron.entity.vector.IntMathVec.IntMathVecFormat

@RunWith(classOf[JUnitRunner])
class InputParserTest extends FunSuite {

  def toLines(s: String) = InputParser.refineLines(s.lines.toStream)

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

  test("read poly from file - empty") {
    val str = // 
      """  
      """
    val (pointList, commonLimits, basis) = InputParser.readPolyFromLines(toLines(str), FracMathVecFormat)
    assert(pointList === s())
    assert(commonLimits === s())
    assert(basis === s())
  }

  test("read poly from file - simplest") {
    val str = // 
      """  
      3
      1 2 3
      4 5 6
      """
    val (pointList, commonLimits, basis) = InputParser.readPolyFromLines(toLines(str), FracMathVecFormat)
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s())
    assert(basis === s())
  }

  test("read poly from file - basis") {
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
    val (pointList, commonLimits, basis) = InputParser.readPolyFromLines(toLines(str), FracMathVecFormat)
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s())
    assert(basis === s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)))
  }

  test("read poly from file - limits") {
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
    val (pointList, commonLimits, basis) = InputParser.readPolyFromLines(toLines(str), FracMathVecFormat)
    assert(pointList === s(fv(1, 2, 3), fv(4, 5, 6)))
    assert(commonLimits === s(
      iv(1, 0, 0),
      iv(0, 0, 1),
      iv(3, 1, -1),
      iv(2, 3, 4)))
    assert(basis === s())
  }

  test("read poly from file - both") {
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
      
      
      """
    val (pointList, commonLimits, basis) = InputParser.readPolyFromLines(toLines(str), FracMathVecFormat)
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
}