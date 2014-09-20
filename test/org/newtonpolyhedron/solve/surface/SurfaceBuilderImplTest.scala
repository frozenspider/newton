package org.newtonpolyhedron.solve.surface

import org.newtonpolyhedron.test._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.solve.cone.ConeSolverImpl
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.fs.utils.collection.table.ArrayListKeyTable
import org.newtonpolyhedron.entity.Surface

@RunWith(classOf[JUnitRunner])
class SurfaceBuilderImplTest extends FunSuite {

  val surfaceBuilder = new SurfaceBuilderImpl()

  test("medium test case") {
    /*-
	   =================| Q0   Q1   Q2   Q3   Q4  
	   N1 = [ 0 0 -1 ]  |  +    +    +    +    -    
	   N2 = [ -2 -2 1 ] |  +    +    -    -    +    
	   N4 = [ -2 2 1 ]  |  -    -    -    +    +    
	   N3 = [ 2 -2 1 ]  |  -    +    +    -    +    
	   N5 = [ 2 2 1 ]   |  -    -    -    -    + 
	 */
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 5)
    val vecs = s(
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
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 2
      s(
        (s(0, 1, 2, 3), s()),
        (s(0, 1, 4), s()),
        (s(0, 3, 4), s()),
        (s(1, 2, 4), s()),
        (s(2, 3, 4), s())),

      // Dimension: 1
      s(
        (s(0, 1), s(0, 1)),
        (s(0, 3), s(0, 2)),
        (s(0, 4), s(1, 2)),
        (s(1, 2), s(0, 3)),
        (s(1, 4), s(1, 3)),
        (s(2, 3), s(0, 4)),
        (s(2, 4), s(3, 4)),
        (s(3, 4), s(2, 4))),

      // Dimension: 0
      s(
        (s(0), s(0, 1, 2)),
        (s(1), s(0, 3, 4)),
        (s(2), s(3, 5, 6)),
        (s(3), s(1, 5, 7)),
        (s(4), s(2, 4, 6, 7)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 3)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("large test case") {
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
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 25)
    val vecs = s(
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
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 2
      s(
        (s(0, 2, 4), s()),
        (s(0, 2, 20, 22), s()),
        (s(0, 3, 4, 5, 8, 9), s()),
        (s(0, 5, 20), s()),
        (s(2, 4, 17, 19), s()),
        (s(2, 17, 22), s()),
        (s(3, 4, 8, 9, 13, 14, 18, 19), s()),
        (s(5, 6, 8), s()),
        (s(5, 6, 20, 21), s()),
        (s(6, 8, 11, 13), s()),
        (s(6, 11, 21), s()),
        (s(11, 12, 13, 14, 16, 17, 18, 19), s()),
        (s(11, 12, 16, 17, 21, 22), s()),
        (s(20, 21, 22), s())),

      // Dimension: 1
      s(
        (s(0, 2), s(0, 1)),
        (s(0, 4), s(0, 2)),
        (s(0, 5), s(2, 3)),
        (s(0, 20), s(1, 3)),
        (s(2, 4), s(0, 4)),
        (s(2, 17), s(4, 5)),
        (s(2, 22), s(1, 5)),
        (s(3, 4, 8, 9), s(2, 6)),
        (s(4, 19), s(4, 6)),
        (s(5, 6), s(7, 8)),
        (s(5, 8), s(2, 7)),
        (s(5, 20), s(3, 8)),
        (s(6, 8), s(7, 9)),
        (s(6, 11), s(9, 10)),
        (s(6, 21), s(8, 10)),
        (s(8, 13), s(6, 9)),
        (s(11, 12, 16, 17), s(11, 12)),
        (s(11, 13), s(9, 11)),
        (s(11, 21), s(10, 12)),
        (s(13, 14, 18, 19), s(6, 11)),
        (s(17, 19), s(4, 11)),
        (s(17, 22), s(5, 12)),
        (s(20, 21), s(8, 13)),
        (s(20, 22), s(1, 13)),
        (s(21, 22), s(12, 13))),

      // Dimension: 0
      s(
        (s(0), s(0, 1, 2, 3)),
        (s(2), s(0, 4, 5, 6)),
        (s(4), s(1, 4, 7, 8)),
        (s(5), s(2, 9, 10, 11)),
        (s(6), s(9, 12, 13, 14)),
        (s(8), s(7, 10, 12, 15)),
        (s(11), s(13, 16, 17, 18)),
        (s(13), s(15, 17, 19)),
        (s(17), s(5, 16, 20, 21)),
        (s(19), s(8, 19, 20)),
        (s(20), s(3, 11, 22, 23)),
        (s(21), s(14, 18, 22, 24)),
        (s(22), s(6, 21, 23, 24)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 3)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("penleve") {
    /*-
	==============| Q0   Q1   Q2   Q3   Q4   Q5   Q6   Q7   Q8   Q9  
	N1 = [ -1 0 ] |  +    +    +    +    +    +    -    -    -    -    
	N2 = [ 1 -1 ] |  -    -    -    -    -    +    -    -    -    +    
	N3 = [ 1 0 ]  |  -    -    -    -    -    -    -    -    +    +    
	N4 = [ 1 1 ]  |  -    -    +    -    -    -    -    -    +    -
	 */
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 9)
    val vecs = s(
      iv(-1, 0),
      iv(1, -1),
      iv(1, 0),
      iv(1, 1))
    val marked = s(
      s(0, 1, 2, 3, 4, 5),
      s(5, 9),
      s(8, 9),
      s(2, 8))
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 1
      s(
        (s(0, 1, 2, 3, 4, 5), s()),
        (s(2, 8), s()),
        (s(5, 9), s()),
        (s(8, 9), s())),

      // Dimension: 0
      s(
        (s(2), s(0, 1)),
        (s(5), s(0, 2)),
        (s(8), s(1, 3)),
        (s(9), s(2, 3)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 2)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("half-cube diagonal") {
    /*-
	================| Q0   Q1   Q2   Q3   Q4   Q5  
	N1 = [ 0 -1 1 ] |  +    +    -    +    -    +    
	N4 = [ 0 0 -1 ] |  +    +    +    -    +    -    
	N2 = [ -1 0 0 ] |  +    -    +    +    -    -    
	N3 = [ 0 1 0  ] |  -    -    +    +    +    +    
	N5 = [ 1 0 0  ] |  -    +    -    -    +    +  
	 */
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 5)
    val vecs = s(
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
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 2
      s(
        (s(0, 1, 2, 4), s()),
        (s(0, 1, 3, 5), s()),
        (s(0, 2, 3), s()),
        (s(1, 4, 5), s()),
        (s(2, 3, 4, 5), s())),

      // Dimension: 1
      s(
        (s(0, 1), s(0, 1)),
        (s(0, 2), s(0, 2)),
        (s(0, 3), s(1, 2)),
        (s(1, 4), s(0, 3)),
        (s(1, 5), s(1, 3)),
        (s(2, 3), s(2, 4)),
        (s(2, 4), s(0, 4)),
        (s(3, 5), s(1, 4)),
        (s(4, 5), s(3, 4))),

      // Dimension: 0
      s(
        (s(0), s(0, 1, 2)),
        (s(1), s(0, 3, 4)),
        (s(2), s(1, 5, 6)),
        (s(3), s(2, 5, 7)),
        (s(4), s(3, 6, 8)),
        (s(5), s(4, 7, 8)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 3)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("Bruno, pages 19 to 30") {
    /*-
	                     | Q0  Q1  Q2  Q3  Q4
	   N1 = [ -2 -1 -1 ] |  +   -   +   +   -
	   N2 = [ -1 -2 -1 ] |  +   +   -   +   +
	   N3 = [ -1 -1 -2 ] |  +   +   +   -   -
	   N4 = [  1  1  1 ] |  -   +   +   +   +
	 */
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 5)
    val vecs = s(
      iv(-2, -1, -1),
      iv(-1, -2, -1),
      iv(-1, -1, -2),
      iv(1, 1, 1))
    val marked = s(
      s(0, 2, 3),
      s(0, 1, 3, 4),
      s(0, 1, 2),
      s(1, 2, 3, 4))
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 2
      s(
        (s(0, 1, 2), s()),
        (s(0, 1, 3, 4), s()),
        (s(0, 2, 3), s()),
        (s(1, 2, 3, 4), s())),

      // Dimension: 1
      s(
        (s(0, 1), s(0, 1)),
        (s(0, 2), s(0, 2)),
        (s(0, 3), s(1, 2)),
        (s(1, 2), s(0, 3)),
        (s(1, 3, 4), s(1, 3)),
        (s(2, 3), s(2, 3))),

      // Dimension: 0
      s(
        (s(0), s(0, 1, 2)),
        (s(1), s(0, 3, 4)),
        (s(2), s(1, 3, 5)),
        (s(3), s(2, 4, 5)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 3)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("Bruno, page 18, ex. 1") {
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
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 12)
    val vecs = s(
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
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 3
      s(
        (s(0, 1, 2, 8, 9, 10, 11), s())),

      // Dimension: 2
      s(
        (s(1, 2, 9, 10, 11), s(0)),
        (s(2, 8, 10, 11), s(0)),
        (s(8, 9, 10), s(0)),
        (s(8, 9, 11), s(0))),

      // Dimension: 1
      s(
        (s(2, 10, 11), s(0, 1)),
        (s(8, 9), s(2, 3)),
        (s(8, 10), s(1, 2)),
        (s(8, 11), s(1, 3)),
        (s(9, 10), s(0, 2)),
        (s(9, 11), s(0, 3))),

      // Dimension: 0
      s(
        (s(8), s(1, 2, 3)),
        (s(9), s(1, 4, 5)),
        (s(10), s(0, 2, 4)),
        (s(11), s(0, 3, 5)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 4)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("Bruno, page 35, ex. 2") {
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
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 12)
    val vecs = s(
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
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 3
      s(
        (s(0, 1, 2, 8, 9, 10, 11), s()),
        (s(1, 2, 4, 9, 10, 11), s()),
        (s(2, 4, 5, 10, 11), s()),
        (s(2, 5, 8, 10, 11), s()),
        (s(4, 5, 6, 7), s()),
        (s(4, 5, 6, 11), s()),
        (s(4, 5, 7, 10), s()),
        (s(4, 6, 7, 9), s()),
        (s(4, 6, 9, 11), s()),
        (s(4, 7, 9, 10), s()),
        (s(5, 6, 7, 8), s()),
        (s(5, 6, 8, 11), s()),
        (s(5, 7, 8, 10), s()),
        (s(6, 7, 8, 9), s()),
        (s(6, 8, 9, 11), s()),
        (s(7, 8, 9, 10), s())),

      // Dimension: 2
      s(
        (s(1, 2, 9, 10, 11), s(0, 1)),
        (s(2, 4, 10, 11), s(1, 2)),
        (s(2, 5, 10, 11), s(2, 3)),
        (s(2, 8, 10, 11), s(0, 3)),
        (s(4, 5, 6), s(4, 5)),
        (s(4, 5, 7), s(4, 6)),
        (s(4, 5, 10), s(2, 6)),
        (s(4, 5, 11), s(2, 5)),
        (s(4, 6, 7), s(4, 7)),
        (s(4, 6, 9), s(7, 8)),
        (s(4, 6, 11), s(5, 8)),
        (s(4, 7, 9), s(7, 9)),
        (s(4, 7, 10), s(6, 9)),
        (s(4, 9, 10), s(1, 9)),
        (s(4, 9, 11), s(1, 8)),
        (s(5, 6, 7), s(4, 10)),
        (s(5, 6, 8), s(10, 11)),
        (s(5, 6, 11), s(5, 11)),
        (s(5, 7, 8), s(10, 12)),
        (s(5, 7, 10), s(6, 12)),
        (s(5, 8, 10), s(3, 12)),
        (s(5, 8, 11), s(3, 11)),
        (s(6, 7, 8), s(10, 13)),
        (s(6, 7, 9), s(7, 13)),
        (s(6, 8, 9), s(13, 14)),
        (s(6, 8, 11), s(11, 14)),
        (s(6, 9, 11), s(8, 14)),
        (s(7, 8, 9), s(13, 15)),
        (s(7, 8, 10), s(12, 15)),
        (s(7, 9, 10), s(9, 15)),
        (s(8, 9, 10), s(0, 15)),
        (s(8, 9, 11), s(0, 14))),

      // Dimension: 1
      s(
        (s(2, 10, 11), s(0, 1, 2, 3)),
        (s(4, 5), s(4, 5, 6, 7)),
        (s(4, 6), s(4, 8, 9, 10)),
        (s(4, 7), s(5, 8, 11, 12)),
        (s(4, 9), s(9, 11, 13, 14)),
        (s(4, 10), s(1, 6, 12, 13)),
        (s(4, 11), s(1, 7, 10, 14)),
        (s(5, 6), s(4, 15, 16, 17)),
        (s(5, 7), s(5, 15, 18, 19)),
        (s(5, 8), s(16, 18, 20, 21)),
        (s(5, 10), s(2, 6, 19, 20)),
        (s(5, 11), s(2, 7, 17, 21)),
        (s(6, 7), s(8, 15, 22, 23)),
        (s(6, 8), s(16, 22, 24, 25)),
        (s(6, 9), s(9, 23, 24, 26)),
        (s(6, 11), s(10, 17, 25, 26)),
        (s(7, 8), s(18, 22, 27, 28)),
        (s(7, 9), s(11, 23, 27, 29)),
        (s(7, 10), s(12, 19, 28, 29)),
        (s(8, 9), s(24, 27, 30, 31)),
        (s(8, 10), s(3, 20, 28, 30)),
        (s(8, 11), s(3, 21, 25, 31)),
        (s(9, 10), s(0, 13, 29, 30)),
        (s(9, 11), s(0, 14, 26, 31))),

      // Dimension: 0
      s(
        (s(4), s(1, 2, 3, 4, 5, 6)),
        (s(5), s(1, 7, 8, 9, 10, 11)),
        (s(6), s(2, 7, 12, 13, 14, 15)),
        (s(7), s(3, 8, 12, 16, 17, 18)),
        (s(8), s(9, 13, 16, 19, 20, 21)),
        (s(9), s(4, 14, 17, 19, 22, 23)),
        (s(10), s(0, 5, 10, 18, 20, 22)),
        (s(11), s(0, 6, 11, 15, 21, 23)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 4)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("Bruno, unknown ex.") {
    // Some random vectors - I haven't found actual ones 
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, 20)
    val vecs = s(
      iv(-1, 0, 0, 0),
      iv(0, -1, 0, 0),
      iv(0, 0, -1, 0),
      iv(0, 0, 0, -1),
      iv(0, 0, 0, 1),
      iv(0, 0, 1, 0),
      iv(0, 1, 0, 0),
      iv(1, 0, 0, 0))
    val marked = s(
      s(1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19),
      s(2, 3, 6, 7, 10, 11, 14, 15, 18, 19),
      s(4, 5, 6, 7, 8, 9, 10, 11),
      s(3, 5, 7, 9, 11, 13, 15, 17, 19),
      s(4, 5, 6, 7, 12, 13, 14, 15),
      s(4, 5, 8, 9, 12, 13, 16, 17),
      s(4, 6, 8, 10, 12, 14, 16, 18),
      s(12, 13, 14, 15, 16, 17, 18, 19))
    (vecs zip marked) map { case (vec, marked) => markInTable(lookupTable)(vec)(marked) }

    val expectedSurfaces = chainSurfaces(s(
      // Dimension: 3
      s(
        (s(1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19), s()),
        (s(2, 3, 6, 7, 10, 11, 14, 15, 18, 19), s()),
        (s(3, 5, 7, 9, 11, 13, 15, 17, 19), s()),
        (s(4, 5, 6, 7, 8, 9, 10, 11), s()),
        (s(4, 5, 6, 7, 12, 13, 14, 15), s()),
        (s(4, 5, 8, 9, 12, 13, 16, 17), s()),
        (s(4, 6, 8, 10, 12, 14, 16, 18), s()),
        (s(12, 13, 14, 15, 16, 17, 18, 19), s())),

      // Dimension: 2
      s(
        (s(2, 3, 10, 11, 18, 19), s(0, 1)),
        (s(3, 7, 11, 15, 19), s(1, 2)),
        (s(3, 9, 11, 17, 19), s(0, 2)),
        (s(4, 5, 6, 7), s(3, 4)),
        (s(4, 5, 8, 9), s(3, 5)),
        (s(4, 5, 12, 13), s(4, 5)),
        (s(4, 6, 8, 10), s(3, 6)),
        (s(4, 6, 12, 14), s(4, 6)),
        (s(4, 8, 12, 16), s(5, 6)),
        (s(5, 7, 9, 11), s(2, 3)),
        (s(5, 7, 13, 15), s(2, 4)),
        (s(5, 9, 13, 17), s(2, 5)),
        (s(6, 7, 10, 11), s(1, 3)),
        (s(6, 7, 14, 15), s(1, 4)),
        (s(6, 10, 14, 18), s(1, 6)),
        (s(8, 9, 10, 11), s(0, 3)),
        (s(8, 9, 16, 17), s(0, 5)),
        (s(8, 10, 16, 18), s(0, 6)),
        (s(12, 13, 14, 15), s(4, 7)),
        (s(12, 13, 16, 17), s(5, 7)),
        (s(12, 14, 16, 18), s(6, 7)),
        (s(14, 15, 18, 19), s(1, 7)),
        (s(16, 17, 18, 19), s(0, 7)),
        (s(13, 15, 17, 19), s(2, 7))),

      // Dimension: 1
      s(
        (s(3, 11, 19), s(0, 1, 2)),
        (s(4, 5), s(3, 4, 5)),
        (s(4, 6), s(3, 6, 7)),
        (s(4, 8), s(4, 6, 8)),
        (s(4, 12), s(5, 7, 8)),
        (s(5, 7), s(3, 9, 10)),
        (s(5, 9), s(4, 9, 11)),
        (s(5, 13), s(5, 10, 11)),
        (s(6, 7), s(3, 12, 13)),
        (s(6, 10), s(6, 12, 14)),
        (s(6, 14), s(7, 13, 14)),
        (s(7, 11), s(1, 9, 12)),
        (s(7, 15), s(1, 10, 13)),
        (s(8, 9), s(4, 15, 16)),
        (s(8, 10), s(6, 15, 17)),
        (s(8, 16), s(8, 16, 17)),
        (s(9, 11), s(2, 9, 15)),
        (s(9, 17), s(2, 11, 16)),
        (s(10, 11), s(0, 12, 15)),
        (s(10, 18), s(0, 14, 17)),
        (s(12, 13), s(5, 18, 19)),
        (s(12, 14), s(7, 18, 20)),
        (s(12, 16), s(8, 19, 20)),
        (s(13, 15), s(10, 18, 21)),
        (s(13, 17), s(11, 19, 21)),
        (s(14, 15), s(13, 18, 22)),
        (s(14, 18), s(14, 20, 22)),
        (s(15, 19), s(1, 21, 22)),
        (s(16, 17), s(16, 19, 23)),
        (s(16, 18), s(17, 20, 23)),
        (s(17, 19), s(2, 21, 23)),
        (s(18, 19), s(0, 22, 23))),

      // Dimension: 0
      s(
        (s(4), s(1, 2, 3, 4)),
        (s(5), s(1, 5, 6, 7)),
        (s(6), s(2, 8, 9, 10)),
        (s(7), s(5, 8, 11, 12)),
        (s(8), s(3, 13, 14, 15)),
        (s(9), s(6, 13, 16, 17)),
        (s(10), s(9, 14, 18, 19)),
        (s(11), s(0, 11, 16, 18)),
        (s(12), s(4, 20, 21, 22)),
        (s(13), s(7, 20, 23, 24)),
        (s(14), s(10, 21, 25, 26)),
        (s(15), s(12, 23, 25, 27)),
        (s(16), s(15, 22, 28, 29)),
        (s(17), s(17, 24, 28, 30)),
        (s(18), s(19, 26, 29, 31)),
        (s(19), s(0, 27, 30, 31)))))

    val actualSurfaces = surfaceBuilder.surfaces(lookupTable, 4)
    assert(actualSurfaces === expectedSurfaces)
  }

  test("illegal cases") {
    val surfaces = Set.empty[Surface]
    val lookupTableData = Seq.empty[Seq[Int]]

    // Poly dimension < 2
    intercept[IllegalArgumentException] {
      surfaceBuilder.findCommonSurfaces(1, 1, surfaces, lookupTableData)
    }

    // Negative target dimension
    intercept[IllegalArgumentException] {
      surfaceBuilder.findCommonSurfaces(3, -1, surfaces, lookupTableData)
    }

    // Target dimension > Poly dimension
    intercept[IllegalArgumentException] {
      surfaceBuilder.findCommonSurfaces(3, 4, surfaces, lookupTableData)
    }

    // Target dimension = Poly dimension
    intercept[IllegalArgumentException] {
      surfaceBuilder.findCommonSurfaces(3, 3, surfaces, lookupTableData)
    }

    // Target dimension < Poly dimension
    // (no exception)
    surfaceBuilder.findCommonSurfaces(3, 2, surfaces, lookupTableData)
  }
}