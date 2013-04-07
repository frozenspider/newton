package org.newtonpolyhedron.solve.poly

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.newtonpolyhedron.solve.cone.ConeSolverImpl
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec
import java.io.PrintWriter
import org.fs.utils.collection.table.KeyTables
import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import java.util.Comparator

@RunWith(classOf[JUnitRunner])
class PolyMotzkinBurgerSolverTest extends FunSuite {

  val solver = new PolyMotzkinBurgerSolver(new ConeSolverImpl)

  def solve(points: IndexedSeq[FracMathVec],
            commonLimits: IndexedSeq[IntMathVec],
            wishfulBasis: IndexedSeq[IntMathVec]) =
    solver.solve(points, commonLimits, wishfulBasis, /* NullPrintWriter.instance */ new PrintWriter(System.out, true))

  test("Bruno, pages 19 to 30") {
    val points = s(
      fv(1, 1, 1),
      fv(4, 0, 0),
      fv(0, 4, 0),
      fv(0, 0, 4),
      fv(2, 0, 2))
    /*-
	   ==================| Q0  Q1  Q2  Q3  Q4
	   N1 = [ -2 -1 -1 ] |  +   -   +   +   -
	   N2 = [ -1 -2 -1 ] |  +   +   -   +   +
	   N3 = [ -1 -1 -2 ] |  +   +   +   -   -
	   N4 = [  1  1  1 ] |  -   +   +   +   +
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 5)
    val expectedVecs = s(
      iv(-2, -1, -1),
      iv(-1, -2, -1),
      iv(-1, -1, -2),
      iv(1, 1, 1))
    val marked = s(
      s(0, 2, 3),
      s(0, 1, 3, 4),
      s(0, 1, 2),
      s(1, 2, 3, 4))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("Bruno, page 35") {
    val points = s(
      fv(-1, -1, -1, -1),
      fv(0, -2, -1, -1),
      fv(0, 0, -2, -2),
      fv(0, 0, 0, 0),
      fv(4, 0, 0, 0),
      fv(0, 4, 0, 0),
      fv(0, 0, 4, 0),
      fv(0, 0, 0, 4),
      fv(-4, 0, 0, 0),
      fv(0, -4, 0, 0),
      fv(0, 0, -4, 0),
      fv(0, 0, 0, -4))
    /*-
	|                    | Q0| Q1| Q2| Q3| Q4| Q5| Q6| Q7| Q8| Q9| Q10| Q11|
	|N0 = [ -1 -1 -1 -1 ]| + | + | + | - | - | - | - | - | + | + | +  | +  |
	|N1 = [ -1 -1 -1 1 ] | - | - | - | - | - | - | - | + | + | + | +  | -  |
	|N2 = [ -1 -1 1 -1 ] | - | - | - | - | - | - | + | - | + | + | -  | +  |
	|N3 = [ -1 -1 1 1 ]  | - | - | - | - | - | - | + | + | + | + | -  | -  |
	|N4 = [ -1 1 -1 -1 ] | - | - | + | - | - | + | - | - | + | - | +  | +  |
	|N5 = [ -1 1 -1 1 ]  | - | - | - | - | - | + | - | + | + | - | +  | -  |
	|N6 = [ -1 1 1 -1 ]  | - | - | - | - | - | + | + | - | + | - | -  | +  |
	|N7 = [ -1 1 1 1 ]   | - | - | - | - | - | + | + | + | + | - | -  | -  |
	|N8 = [ 1 -1 -1 -1 ] | - | + | + | - | + | - | - | - | - | + | +  | +  |
	|N9 = [ 1 -1 -1 1 ]  | - | - | - | - | + | - | - | + | - | + | +  | -  |
	|N10 = [ 1 -1 1 -1 ] | - | - | - | - | + | - | + | - | - | + | -  | +  |
	|N11 = [ 1 -1 1 1 ]  | - | - | - | - | + | - | + | + | - | + | -  | -  |
	|N12 = [ 1 1 -1 -1 ] | - | - | + | - | + | + | - | - | - | - | +  | +  |
	|N13 = [ 1 1 -1 1 ]  | - | - | - | - | + | + | - | + | - | - | +  | -  |
	|N14 = [ 1 1 1 -1 ]  | - | - | - | - | + | + | + | - | - | - | -  | +  |
	|N15 = [ 1 1 1 1 ]   | - | - | - | - | + | + | + | + | - | - | -  | -  |
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 12)
    def mark = markInTable(expectedLookupTable)_
    val expectedVecs = s(
      iv(-1, -1, -1, -1),
      iv(-1, -1, -1, 1),
      iv(-1, -1, 1, -1),
      iv(-1, -1, 1, 1),
      iv(-1, 1, -1, -1),
      iv(-1, 1, -1, 1),
      iv(-1, 1, 1, -1),
      iv(-1, 1, 1, 1),
      iv(1, -1, -1, -1),
      iv(1, -1, -1, 1),
      iv(1, -1, 1, -1),
      iv(1, -1, 1, 1),
      iv(1, 1, -1, -1),
      iv(1, 1, -1, 1),
      iv(1, 1, 1, -1),
      iv(1, 1, 1, 1))
    val marked = s(
      s(0, 1, 2, 8, 9, 10, 11),
      s(7, 8, 9, 10),
      s(6, 8, 9, 11),
      s(6, 7, 8, 9),
      s(2, 5, 8, 10, 11),
      s(5, 7, 8, 10),
      s(5, 6, 8, 11),
      s(5, 6, 7, 8),
      s(1, 2, 4, 9, 10, 11),
      s(4, 7, 9, 10),
      s(4, 6, 9, 11),
      s(4, 6, 7, 9),
      s(2, 4, 5, 10, 11),
      s(4, 5, 7, 10),
      s(4, 5, 6, 11),
      s(4, 5, 6, 7))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("Bruno, pages 38 [with common limits]") {
    val points = s(
      fv(-1, -1, -1, -1),
      fv(0, -2, -1, -1),
      fv(0, 0, -2, -2),
      fv(0, 0, 0, 0),
      fv(4, 0, 0, 0),
      fv(0, 4, 0, 0),
      fv(0, 0, 4, 0),
      fv(0, 0, 0, 4),
      fv(-4, 0, 0, 0),
      fv(0, -4, 0, 0),
      fv(0, 0, -4, 0),
      fv(0, 0, 0, -4))
    val commonLimits = s(
      iv(1, 0, 0, 0),
      iv(0, 1, 0, 0),
      iv(0, 0, 1, 0),
      iv(0, 0, 0, 1))
    /*-
	|                    | Q0| Q1| Q2| Q3| Q4| Q5| Q6| Q7| Q8| Q9| Q10| Q11|
	|N0 = [ -1 -1 -1 -1 ]| + | + | + | - | - | - | - | - | + | + | +  | +  |
	|N1 = [ -1 -1 -1 0 ] | - | - | - | - | - | - | - | - | + | + | +  | -  |
	|N2 = [ -1 -1 0 -1 ] | - | - | - | - | - | - | - | - | + | + | -  | +  |
	|N3 = [ -1 -1 0 0 ]  | - | - | - | - | - | - | - | - | + | + | -  | -  |
	|N4 = [ -1 0 -1 -1 ] | - | - | + | - | - | - | - | - | + | - | +  | +  |
	|N5 = [ -1 0 -1 0 ]  | - | - | - | - | - | - | - | - | + | - | +  | -  |
	|N6 = [ -1 0 0 -1 ]  | - | - | - | - | - | - | - | - | + | - | -  | +  |
	|N7 = [ -1 0 0 0 ]   | - | - | - | - | - | - | - | - | + | - | -  | -  |
	|N8 = [ 0 -1 -1 -1 ] | - | + | + | - | - | - | - | - | - | + | +  | +  |
	|N9 = [ 0 -1 -1 0 ]  | - | - | - | - | - | - | - | - | - | + | +  | -  |
	|N10 = [ 0 -1 0 -1 ] | - | - | - | - | - | - | - | - | - | + | -  | +  |
	|N11 = [ 0 -1 0 0 ]  | - | - | - | - | - | - | - | - | - | + | -  | -  |
	|N12 = [ 0 0 -1 -1 ] | - | - | + | - | - | - | - | - | - | - | +  | +  |
	|N13 = [ 0 0 -1 0 ]  | - | - | - | - | - | - | - | - | - | - | +  | -  |
	|N14 = [ 0 0 0 -1 ]  | - | - | - | - | - | - | - | - | - | - | -  | +  |
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 12)
    val expectedVecs = s(
      iv(-1, -1, -1, -1),
      iv(-1, -1, -1, 0),
      iv(-1, -1, 0, -1),
      iv(-1, -1, 0, 0),
      iv(-1, 0, -1, -1),
      iv(-1, 0, -1, 0),
      iv(-1, 0, 0, -1),
      iv(-1, 0, 0, 0),
      iv(0, -1, -1, -1),
      iv(0, -1, -1, 0),
      iv(0, -1, 0, -1),
      iv(0, -1, 0, 0),
      iv(0, 0, -1, -1),
      iv(0, 0, -1, 0),
      iv(0, 0, 0, -1))
    val marked = s(
      s(0, 1, 2, 8, 9, 10, 11),
      s(8, 9, 10),
      s(8, 9, 11),
      s(8, 9),
      s(2, 8, 10, 11),
      s(8, 10),
      s(8, 11),
      s(8),
      s(1, 2, 9, 10, 11),
      s(9, 10),
      s(9, 11),
      s(9),
      s(2, 10, 11),
      s(10),
      s(11))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, commonLimits, s())
    assert(lookupTable === expectedLookupTable)
  }

  test("medium test case") {
    val points = s(
      fv(0, 2, 0),
      fv(2, 0, 0),
      fv(4, 2, 0),
      fv(2, 4, 0),
      fv(2, 2, 4))
    /*-
	   =================| Q0   Q1   Q2   Q3   Q4  
	   N1 = [ 0 0 -1 ]  |  +    +    +    +    -    
	   N2 = [ -2 -2 1 ] |  +    +    -    -    +    
	   N4 = [ -2 2 1 ]  |  -    -    -    +    +    
	   N3 = [ 2 -2 1 ]  |  -    +    +    -    +    
	   N5 = [ 2 2 1 ]   |  -    -    -    -    + 
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 5)
    val expectedVecs = s(
      iv(0, 0, -1),
      iv(-2, -2, 1),
      iv(-2, 2, 1),
      iv(2, -2, 1),
      iv(2, 2, 1))
    val marked = s(
      s(0, 1, 2, 3),
      s(0, 1, 4),
      s(0, 3, 4),
      s(1, 2, 4),
      s(2, 3, 4))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("large test case") {
    val points = s(
      fv(1, 3, 1),
      fv(4, 2, 3),
      fv(0, 6, 3),
      fv(4, 2, 0),
      fv(0, 6, 0),
      fv(3, 1, 1),
      fv(6, 0, 3),
      fv(2, 4, 3),
      fv(6, 0, 0),
      fv(2, 4, 0),
      fv(5, 3, 1),
      fv(8, 2, 3),
      fv(4, 6, 3),
      fv(8, 2, 0),
      fv(4, 6, 0),
      fv(3, 5, 1),
      fv(6, 4, 3),
      fv(2, 8, 3),
      fv(6, 4, 0),
      fv(2, 8, 0),
      fv(3, 3, 5),
      fv(6, 2, 7),
      fv(2, 6, 7),
      fv(6, 2, 4),
      fv(2, 6, 4))
    /*-
	   |                 | Q0| Q1| Q2| Q3| Q4| Q5| Q6| Q7| Q8| Q9| Q10| Q11| Q12| Q13| Q14| Q15| Q16| Q17| Q18| Q19| Q20| Q21| Q22| Q23| Q24|
	   |N0 = [ -6 -4 3 ] | + | - | + | - | - | - | - | - | - | - | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | +  | -  | +  | -  | -  |
	   |N1 = [ -1 -1 -2 ]| + | - | - | + | + | + | - | - | + | + | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  |
	   |N2 = [ -3 -1 0 ] | + | - | + | - | + | - | - | - | - | - | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  |
	   |N3 = [ -2 -2 1 ] | + | - | - | - | - | + | - | - | - | - | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | +  | -  | -  | -  | -  |
	   |N4 = [ -1 1 0 ]  | - | - | + | - | + | - | - | - | - | - | -  | -  | -  | -  | -  | -  | -  | +  | -  | +  | -  | -  | -  | -  | -  |
	   |N5 = [ -2 2 1 ]  | - | - | + | - | - | - | - | - | - | - | -  | -  | -  | -  | -  | -  | -  | +  | -  | -  | -  | -  | +  | -  | -  |
	   |N6 = [ 0 0 -1 ]  | - | - | - | + | + | - | - | - | + | + | -  | -  | -  | +  | +  | -  | -  | -  | +  | +  | -  | -  | -  | -  | -  |
	   |N7 = [ -4 -6 3 ] | - | - | - | - | - | + | + | - | - | - | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | +  | +  | -  | -  | -  |
	   |N8 = [ -1 -3 0 ] | - | - | - | - | - | + | + | - | + | - | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  |
	   |N9 = [ 1 -1 0 ]  | - | - | - | - | - | - | + | - | + | - | -  | +  | -  | +  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  |
	   |N10 = [ 2 -2 1 ] | - | - | - | - | - | - | + | - | - | - | -  | +  | -  | -  | -  | -  | -  | -  | -  | -  | -  | +  | -  | -  | -  |
	   |N11 = [ 2 2 1 ]  | - | - | - | - | - | - | - | - | - | - | -  | +  | +  | -  | -  | -  | +  | +  | -  | -  | -  | +  | +  | -  | -  |
	   |N12 = [ 1 1 0 ]  | - | - | - | - | - | - | - | - | - | - | -  | +  | +  | +  | +  | -  | +  | +  | +  | +  | -  | -  | -  | -  | -  |
	   |N13 = [ -1 -1 1 ]| - | - | - | - | - | - | - | - | - | - | -  | -  | -  | -  | -  | -  | -  | -  | -  | -  | +  | +  | +  | -  | -  |
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 25)
    val expectedVecs = s(
      iv(-6, -4, 3),
      iv(-4, -6, 3),
      iv(-3, -1, 0),
      iv(-2, -2, 1),
      iv(-2, 2, 1),
      iv(-1, -3, 0),
      iv(-1, -1, -2),
      iv(-1, -1, 1),
      iv(-1, 1, 0),
      iv(0, 0, -1),
      iv(1, -1, 0),
      iv(1, 1, 0),
      iv(2, -2, 1),
      iv(2, 2, 1))
    val marked = s(
      s(0, 2, 20, 22),
      s(5, 6, 20, 21),
      s(0, 2, 4),
      s(0, 5, 20),
      s(2, 17, 22),
      s(5, 6, 8),
      s(0, 3, 4, 5, 8, 9),
      s(20, 21, 22),
      s(2, 4, 17, 19),
      s(3, 4, 8, 9, 13, 14, 18, 19),
      s(6, 8, 11, 13),
      s(11, 12, 13, 14, 16, 17, 18, 19),
      s(6, 11, 21),
      s(11, 12, 16, 17, 21, 22))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("penleve 1") {
    val points = s(
      fv(0, 3),
      fv(0, 2),
      fv(0, 5),
      fv(0, 4),
      fv(0, 1),
      fv(0, 0),
      fv(1, 3),
      fv(1, 2),
      fv(2, 3),
      fv(2, 2))
    /*-
	=== Normal vectors: ===
	==============| Q0   Q1   Q2   Q3   Q4   Q5   Q6   Q7   Q8   Q9  
	N1 = [ -1 0 ] |  +    +    +    +    +    +    -    -    -    -    
	N2 = [ 1 -1 ] |  -    -    -    -    -    +    -    -    -    +    
	N3 = [ 1 0 ]  |  -    -    -    -    -    -    -    -    +    +    
	N4 = [ 1 1 ]  |  -    -    +    -    -    -    -    -    +    -
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 9)
    val expectedVecs = s(
      iv(-1, 0),
      iv(1, -1),
      iv(1, 0),
      iv(1, 1))
    val marked = s(
      s(0, 1, 2, 3, 4, 5),
      s(5, 9),
      s(8, 9),
      s(2, 8))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("half-cube diagonal") {
    val points = s(
      fv(1, 1, 1),
      fv(5, 1, 1),
      fv(1, 5, 1),
      fv(1, 5, 5),
      fv(5, 5, 1),
      fv(5, 5, 5))
    /*-
	================| Q0   Q1   Q2   Q3   Q4   Q5  
	N1 = [ 0 -1 1 ] |  +    +    -    +    -    +    
	N4 = [ 0 0 -1 ] |  +    +    +    -    +    -    
	N2 = [ -1 0 0 ] |  +    -    +    +    -    -    
	N3 = [ 0 1 0  ] |  -    -    +    +    +    +    
	N5 = [ 1 0 0  ] |  -    +    -    -    +    +  
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 5)
    val expectedVecs = s(
      iv(0, -1, 1),
      iv(0, 0, -1),
      iv(-1, 0, 0),
      iv(0, 1, 0),
      iv(1, 0, 0))
    val marked = s(
      s(0, 1, 3, 5),
      s(0, 1, 2, 4),
      s(0, 2, 3),
      s(2, 3, 4, 5),
      s(1, 4, 5))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("half-cube diagonal, decimal") {
    val points = s(
      fv2(bf(1, 2), bf(1, 2), bf(1, 2)),
      fv2(bf(5, 2), bf(1, 2), bf(1, 2)),
      fv2(bf(1, 2), bf(5, 2), bf(1, 2)),
      fv2(bf(1, 2), bf(5, 2), bf(5, 2)),
      fv2(bf(5, 2), bf(5, 2), bf(1, 2)),
      fv2(bf(5, 2), bf(5, 2), bf(5, 2)))
    /*-
	================| Q0   Q1   Q2   Q3   Q4   Q5  
	N1 = [ 0 -1 1 ] |  +    +    -    +    -    +    
	N4 = [ 0 0 -1 ] |  +    +    +    -    +    -    
	N2 = [ -1 0 0 ] |  +    -    +    +    -    -    
	N3 = [ 0 1 0  ] |  -    -    +    +    +    +    
	N5 = [ 1 0 0  ] |  -    +    -    -    +    +  
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 5)
    val expectedVecs = s(
      iv(0, -1, 1),
      iv(0, 0, -1),
      iv(-1, 0, 0),
      iv(0, 1, 0),
      iv(1, 0, 0))
    val marked = s(
      s(0, 1, 3, 5),
      s(0, 1, 2, 4),
      s(0, 2, 3),
      s(2, 3, 4, 5),
      s(1, 4, 5))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("degenerate - flat 4-pts triangle in 3d") {
    val points = s(
      fv(3, 0, 0),
      fv(0, 3, 0),
      fv(0, 0, 3),
      fv(1, 1, 1))
    /*-
	 +------------+----+----+----+----+
	 |            |0   |1   |2   |3   |
	 +------------+----+----+----+----+
	 |[ -1 -1 -1 ]|true|true|true|true|
	 |[ 1 1 1 ]   |true|true|true|true|
	 +------------+----+----+----+----+
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 4)
    val expectedVecs = s(
      iv(-1, -1, -1),
      iv(1, 1, 1))
    val marked = s(
      s(0, 1, 2, 3),
      s(0, 1, 2, 3))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }

  test("degenerate - flat 4-pts triangle in 3d, case 2") {
    val points = s(
      fv(3, -1, -1),
      fv(-1, 3, -1),
      fv(-1, -1, 3),
      fv(1, -1, 1))
    /*-
	 +------------+----+----+----+----+
	 |            |0   |1   |2   |3   |
	 +------------+----+----+----+----+
	 |[ -1 -1 -1 ]|true|true|true|true|
	 |[ 1 1 1 ]   |true|true|true|true|
	 +------------+----+----+----+----+
	 */
    val expectedLookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(expectedLookupTable, 4)
    val expectedVecs = s(
      iv(-1, -1, -1),
      iv(1, 1, 1))
    val marked = s(
      s(0, 1, 2, 3),
      s(0, 1, 2, 3))
    (expectedVecs zip marked) map { case (vec, marked) => markInTable(expectedLookupTable)(vec)(marked) }

    val lookupTable = solve(points, s(), s())
    assert(lookupTable === expectedLookupTable)
  }
}