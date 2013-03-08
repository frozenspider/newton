package org.newtonpolyhedron.solve.poly;

import static org.junit.Assert.*;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.math3.fraction.BigFraction;
import org.fs.utils.collection.table.ArrayListKeyTable;
import org.fs.utils.collection.table.KeyTable;
import org.fs.utils.collection.table.KeyTables;
import org.junit.Test;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.cone.ConeSolverImpl;

public class PolyMotzkinBurgerSolverTest {
	
	private static PolyhedronSolver	solver	= new PolyMotzkinBurgerSolver(new ConeSolverImpl());
	
	@Test
	public void brunoPage19Through30() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(1, 1, 1));
		pointList.add(new FractionVector(4, 0, 0));
		pointList.add(new FractionVector(0, 4, 0));
		pointList.add(new FractionVector(0, 0, 4));
		pointList.add(new FractionVector(2, 0, 2));
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
		/*-
		   ==================| Q0  Q1  Q2  Q3  Q4
		   N1 = [ -2 -1 -1 ] |  +   -   +   +   -
		   N2 = [ -1 -2 -1 ] |  +   +   -   +   +
		   N3 = [ -1 -1 -2 ] |  +   +   +   -   -
		   N4 = [  1  1  1 ] |  -   +   +   +   +
		 */
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 5);
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		
		expectedVecs.add(new IntVector(-2, -1, -1));
		expectedVecs.add(new IntVector(-1, -2, -1));
		expectedVecs.add(new IntVector(-1, -1, -2));
		expectedVecs.add(new IntVector(1, 1, 1));
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 2, 3);
		markInTable(expectedLookupTable, expectedVecs.get(1), 0, 1, 3, 4);
		markInTable(expectedLookupTable, expectedVecs.get(2), 0, 1, 2);
		markInTable(expectedLookupTable, expectedVecs.get(3), 1, 2, 3, 4);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void brunoPage35() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(-1, -1, -1, -1));
		pointList.add(new FractionVector(0, -2, -1, -1));
		pointList.add(new FractionVector(0, 0, -2, -2));
		pointList.add(new FractionVector(0, 0, 0, 0));
		pointList.add(new FractionVector(4, 0, 0, 0));
		pointList.add(new FractionVector(0, 4, 0, 0));
		pointList.add(new FractionVector(0, 0, 4, 0));
		pointList.add(new FractionVector(0, 0, 0, 4));
		pointList.add(new FractionVector(-4, 0, 0, 0));
		pointList.add(new FractionVector(0, -4, 0, 0));
		pointList.add(new FractionVector(0, 0, -4, 0));
		pointList.add(new FractionVector(0, 0, 0, -4));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
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
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 12);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(-1, -1, -1, -1));
		expectedVecs.add(new IntVector(-1, -1, -1, 1));
		expectedVecs.add(new IntVector(-1, -1, 1, -1));
		expectedVecs.add(new IntVector(-1, -1, 1, 1));
		expectedVecs.add(new IntVector(-1, 1, -1, -1));
		expectedVecs.add(new IntVector(-1, 1, -1, 1));
		expectedVecs.add(new IntVector(-1, 1, 1, -1));
		expectedVecs.add(new IntVector(-1, 1, 1, 1));
		expectedVecs.add(new IntVector(1, -1, -1, -1));
		expectedVecs.add(new IntVector(1, -1, -1, 1));
		expectedVecs.add(new IntVector(1, -1, 1, -1));
		expectedVecs.add(new IntVector(1, -1, 1, 1));
		expectedVecs.add(new IntVector(1, 1, -1, -1));
		expectedVecs.add(new IntVector(1, 1, -1, 1));
		expectedVecs.add(new IntVector(1, 1, 1, -1));
		expectedVecs.add(new IntVector(1, 1, 1, 1));
		
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 1, 2, 8, 9, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(1), 7, 8, 9, 10);
		markInTable(expectedLookupTable, expectedVecs.get(2), 6, 8, 9, 11);
		markInTable(expectedLookupTable, expectedVecs.get(3), 6, 7, 8, 9);
		markInTable(expectedLookupTable, expectedVecs.get(4), 2, 5, 8, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(5), 5, 7, 8, 10);
		markInTable(expectedLookupTable, expectedVecs.get(6), 5, 6, 8, 11);
		markInTable(expectedLookupTable, expectedVecs.get(7), 5, 6, 7, 8);
		markInTable(expectedLookupTable, expectedVecs.get(8), 1, 2, 4, 9, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(9), 4, 7, 9, 10);
		markInTable(expectedLookupTable, expectedVecs.get(10), 4, 6, 9, 11);
		markInTable(expectedLookupTable, expectedVecs.get(11), 4, 6, 7, 9);
		markInTable(expectedLookupTable, expectedVecs.get(12), 2, 4, 5, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(13), 4, 5, 7, 10);
		markInTable(expectedLookupTable, expectedVecs.get(14), 4, 5, 6, 11);
		markInTable(expectedLookupTable, expectedVecs.get(15), 4, 5, 6, 7);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void brunoPage38() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(-1, -1, -1, -1));
		pointList.add(new FractionVector(0, -2, -1, -1));
		pointList.add(new FractionVector(0, 0, -2, -2));
		pointList.add(new FractionVector(0, 0, 0, 0));
		pointList.add(new FractionVector(4, 0, 0, 0));
		pointList.add(new FractionVector(0, 4, 0, 0));
		pointList.add(new FractionVector(0, 0, 4, 0));
		pointList.add(new FractionVector(0, 0, 0, 4));
		pointList.add(new FractionVector(-4, 0, 0, 0));
		pointList.add(new FractionVector(0, -4, 0, 0));
		pointList.add(new FractionVector(0, 0, -4, 0));
		pointList.add(new FractionVector(0, 0, 0, -4));
		
		List <IntVector> commonLimits = new ArrayList <IntVector>();
		commonLimits.add(new IntVector(1, 0, 0, 0));
		commonLimits.add(new IntVector(0, 1, 0, 0));
		commonLimits.add(new IntVector(0, 0, 1, 0));
		commonLimits.add(new IntVector(0, 0, 0, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, commonLimits, null);
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
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 12);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(-1, -1, -1, -1));
		expectedVecs.add(new IntVector(-1, -1, -1, 0));
		expectedVecs.add(new IntVector(-1, -1, 0, -1));
		expectedVecs.add(new IntVector(-1, -1, 0, 0));
		expectedVecs.add(new IntVector(-1, 0, -1, -1));
		expectedVecs.add(new IntVector(-1, 0, -1, 0));
		expectedVecs.add(new IntVector(-1, 0, 0, -1));
		expectedVecs.add(new IntVector(-1, 0, 0, 0));
		expectedVecs.add(new IntVector(0, -1, -1, -1));
		expectedVecs.add(new IntVector(0, -1, -1, 0));
		expectedVecs.add(new IntVector(0, -1, 0, -1));
		expectedVecs.add(new IntVector(0, -1, 0, 0));
		expectedVecs.add(new IntVector(0, 0, -1, -1));
		expectedVecs.add(new IntVector(0, 0, -1, 0));
		expectedVecs.add(new IntVector(0, 0, 0, -1));
		
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 1, 2, 8, 9, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(1), 8, 9, 10);
		markInTable(expectedLookupTable, expectedVecs.get(2), 8, 9, 11);
		markInTable(expectedLookupTable, expectedVecs.get(3), 8, 9);
		markInTable(expectedLookupTable, expectedVecs.get(4), 2, 8, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(5), 8, 10);
		markInTable(expectedLookupTable, expectedVecs.get(6), 8, 11);
		markInTable(expectedLookupTable, expectedVecs.get(7), 8);
		markInTable(expectedLookupTable, expectedVecs.get(8), 1, 2, 9, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(9), 9, 10);
		markInTable(expectedLookupTable, expectedVecs.get(10), 9, 11);
		markInTable(expectedLookupTable, expectedVecs.get(11), 9);
		markInTable(expectedLookupTable, expectedVecs.get(12), 2, 10, 11);
		markInTable(expectedLookupTable, expectedVecs.get(13), 10);
		markInTable(expectedLookupTable, expectedVecs.get(14), 11);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void mediumTestCase() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(0, 2, 0));
		pointList.add(new FractionVector(2, 0, 0));
		pointList.add(new FractionVector(4, 2, 0));
		pointList.add(new FractionVector(2, 4, 0));
		pointList.add(new FractionVector(2, 2, 4));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
		/*-
		   =================| Q0   Q1   Q2   Q3   Q4  
		   N1 = [ 0 0 -1 ]  |  +    +    +    +    -    
		   N2 = [ -2 -2 1 ] |  +    +    -    -    +    
		   N4 = [ -2 2 1 ]  |  -    -    -    +    +    
		   N3 = [ 2 -2 1 ]  |  -    +    +    -    +    
		   N5 = [ 2 2 1 ]   |  -    -    -    -    + 
		 */
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 5);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(0, 0, -1));
		expectedVecs.add(new IntVector(-2, -2, 1));
		expectedVecs.add(new IntVector(-2, 2, 1));
		expectedVecs.add(new IntVector(2, -2, 1));
		expectedVecs.add(new IntVector(2, 2, 1));
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 1, 2, 3);
		markInTable(expectedLookupTable, expectedVecs.get(1), 0, 1, 4);
		markInTable(expectedLookupTable, expectedVecs.get(2), 0, 3, 4);
		markInTable(expectedLookupTable, expectedVecs.get(3), 1, 2, 4);
		markInTable(expectedLookupTable, expectedVecs.get(4), 2, 3, 4);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void largeTestCase() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(1, 3, 1));
		pointList.add(new FractionVector(4, 2, 3));
		pointList.add(new FractionVector(0, 6, 3));
		pointList.add(new FractionVector(4, 2, 0));
		pointList.add(new FractionVector(0, 6, 0));
		pointList.add(new FractionVector(3, 1, 1));
		pointList.add(new FractionVector(6, 0, 3));
		pointList.add(new FractionVector(2, 4, 3));
		pointList.add(new FractionVector(6, 0, 0));
		pointList.add(new FractionVector(2, 4, 0));
		pointList.add(new FractionVector(5, 3, 1));
		pointList.add(new FractionVector(8, 2, 3));
		pointList.add(new FractionVector(4, 6, 3));
		pointList.add(new FractionVector(8, 2, 0));
		pointList.add(new FractionVector(4, 6, 0));
		pointList.add(new FractionVector(3, 5, 1));
		pointList.add(new FractionVector(6, 4, 3));
		pointList.add(new FractionVector(2, 8, 3));
		pointList.add(new FractionVector(6, 4, 0));
		pointList.add(new FractionVector(2, 8, 0));
		pointList.add(new FractionVector(3, 3, 5));
		pointList.add(new FractionVector(6, 2, 7));
		pointList.add(new FractionVector(2, 6, 7));
		pointList.add(new FractionVector(6, 2, 4));
		pointList.add(new FractionVector(2, 6, 4));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
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
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 25);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(-6, -4, 3));
		expectedVecs.add(new IntVector(-4, -6, 3));
		expectedVecs.add(new IntVector(-3, -1, 0));
		expectedVecs.add(new IntVector(-2, -2, 1));
		expectedVecs.add(new IntVector(-2, 2, 1));
		expectedVecs.add(new IntVector(-1, -3, 0));
		expectedVecs.add(new IntVector(-1, -1, -2));
		expectedVecs.add(new IntVector(-1, -1, 1));
		expectedVecs.add(new IntVector(-1, 1, 0));
		expectedVecs.add(new IntVector(0, 0, -1));
		expectedVecs.add(new IntVector(1, -1, 0));
		expectedVecs.add(new IntVector(1, 1, 0));
		expectedVecs.add(new IntVector(2, -2, 1));
		expectedVecs.add(new IntVector(2, 2, 1));
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 2, 20, 22);
		markInTable(expectedLookupTable, expectedVecs.get(1), 5, 6, 20, 21);
		markInTable(expectedLookupTable, expectedVecs.get(2), 0, 2, 4);
		markInTable(expectedLookupTable, expectedVecs.get(3), 0, 5, 20);
		markInTable(expectedLookupTable, expectedVecs.get(4), 2, 17, 22);
		markInTable(expectedLookupTable, expectedVecs.get(5), 5, 6, 8);
		markInTable(expectedLookupTable, expectedVecs.get(6), 0, 3, 4, 5, 8, 9);
		markInTable(expectedLookupTable, expectedVecs.get(7), 20, 21, 22);
		markInTable(expectedLookupTable, expectedVecs.get(8), 2, 4, 17, 19);
		markInTable(expectedLookupTable, expectedVecs.get(9), 3, 4, 8, 9, 13, 14, 18, 19);
		markInTable(expectedLookupTable, expectedVecs.get(10), 6, 8, 11, 13);
		markInTable(expectedLookupTable, expectedVecs.get(11), 11, 12, 13, 14, 16, 17, 18, 19);
		markInTable(expectedLookupTable, expectedVecs.get(12), 6, 11, 21);
		markInTable(expectedLookupTable, expectedVecs.get(13), 11, 12, 16, 17, 21, 22);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void penleve1() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(0, 3));
		pointList.add(new FractionVector(0, 2));
		pointList.add(new FractionVector(0, 5));
		pointList.add(new FractionVector(0, 4));
		pointList.add(new FractionVector(0, 1));
		pointList.add(new FractionVector(0, 0));
		pointList.add(new FractionVector(1, 3));
		pointList.add(new FractionVector(1, 2));
		pointList.add(new FractionVector(2, 3));
		pointList.add(new FractionVector(2, 2));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
		/*-
		=== Normal vectors: ===
		==============| Q0   Q1   Q2   Q3   Q4   Q5   Q6   Q7   Q8   Q9  
		N1 = [ -1 0 ] |  +    +    +    +    +    +    -    -    -    -    
		N2 = [ 1 -1 ] |  -    -    -    -    -    +    -    -    -    +    
		N3 = [ 1 0 ]  |  -    -    -    -    -    -    -    -    +    +    
		N4 = [ 1 1 ]  |  -    -    +    -    -    -    -    -    +    -
		 */
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 9);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(-1, 0));
		expectedVecs.add(new IntVector(1, -1));
		expectedVecs.add(new IntVector(1, 0));
		expectedVecs.add(new IntVector(1, 1));
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 1, 2, 3, 4, 5);
		markInTable(expectedLookupTable, expectedVecs.get(1), 5, 9);
		markInTable(expectedLookupTable, expectedVecs.get(2), 8, 9);
		markInTable(expectedLookupTable, expectedVecs.get(3), 2, 8);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void halfCubeDiag() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(1, 1, 1));
		pointList.add(new FractionVector(5, 1, 1));
		pointList.add(new FractionVector(1, 5, 1));
		pointList.add(new FractionVector(1, 5, 5));
		pointList.add(new FractionVector(5, 5, 1));
		pointList.add(new FractionVector(5, 5, 5));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
		/*-
		================| Q0   Q1   Q2   Q3   Q4   Q5  
		N1 = [ 0 -1 1 ] |  +    +    -    +    -    +    
		N4 = [ 0 0 -1 ] |  +    +    +    -    +    -    
		N2 = [ -1 0 0 ] |  +    -    +    +    -    -    
		N3 = [ 0 1 0  ] |  -    -    +    +    +    +    
		N5 = [ 1 0 0  ] |  -    +    -    -    +    +  
		 */
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 5);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(0, -1, 1));
		expectedVecs.add(new IntVector(0, 0, -1));
		expectedVecs.add(new IntVector(-1, 0, 0));
		expectedVecs.add(new IntVector(0, 1, 0));
		expectedVecs.add(new IntVector(1, 0, 0));
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 1, 3, 5);
		markInTable(expectedLookupTable, expectedVecs.get(1), 0, 1, 2, 4);
		markInTable(expectedLookupTable, expectedVecs.get(2), 0, 2, 3);
		markInTable(expectedLookupTable, expectedVecs.get(3), 2, 3, 4, 5);
		markInTable(expectedLookupTable, expectedVecs.get(4), 1, 4, 5);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void halfCubeDiagDecimal() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(bf(1, 2), bf(1, 2), bf(1, 2)));
		pointList.add(new FractionVector(bf(5, 2), bf(1, 2), bf(1, 2)));
		pointList.add(new FractionVector(bf(1, 2), bf(5, 2), bf(1, 2)));
		pointList.add(new FractionVector(bf(1, 2), bf(5, 2), bf(5, 2)));
		pointList.add(new FractionVector(bf(5, 2), bf(5, 2), bf(1, 2)));
		pointList.add(new FractionVector(bf(5, 2), bf(5, 2), bf(5, 2)));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
		/*-
		================| Q0   Q1   Q2   Q3   Q4   Q5  
		N1 = [ 0 -1 1 ] |  +    +    -    +    -    +    
		N4 = [ 0 0 -1 ] |  +    +    +    -    +    -    
		N2 = [ -1 0 0 ] |  +    -    +    +    -    -    
		N3 = [ 0 1 0  ] |  -    -    +    +    +    +    
		N5 = [ 1 0 0  ] |  -    +    -    -    +    +  
		 */
		KeyTable <IntVector, Integer, Boolean> expectedLookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(expectedLookupTable, 5);
		
		List <IntVector> expectedVecs = new ArrayList <IntVector>();
		expectedVecs.add(new IntVector(0, -1, 1));
		expectedVecs.add(new IntVector(0, 0, -1));
		expectedVecs.add(new IntVector(-1, 0, 0));
		expectedVecs.add(new IntVector(0, 1, 0));
		expectedVecs.add(new IntVector(1, 0, 0));
		markInTable(expectedLookupTable, expectedVecs.get(0), 0, 1, 3, 5);
		markInTable(expectedLookupTable, expectedVecs.get(1), 0, 1, 2, 4);
		markInTable(expectedLookupTable, expectedVecs.get(2), 0, 2, 3);
		markInTable(expectedLookupTable, expectedVecs.get(3), 2, 3, 4, 5);
		markInTable(expectedLookupTable, expectedVecs.get(4), 1, 4, 5);
		KeyTables.sortByColHeaders(expectedLookupTable, true);
		assertEquals(expectedLookupTable, lookupTable);
	}
	
	@Test
	public void degenerateFlatFourPtsTriangleIn3d() throws Exception {
		List <FractionVector> pointList = new ArrayList <FractionVector>();
		pointList.add(new FractionVector(3, 0, 0));
		pointList.add(new FractionVector(0, 3, 0));
		pointList.add(new FractionVector(0, 0, 3));
		pointList.add(new FractionVector(1, 1, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = doSolve(pointList, null, null);
		
		System.out.println(lookupTable);
		fail("Implement me!");
	}
	
	//
	// Supportive
	//
	private static BigFraction bf(int num, int denom) {
		return new BigFraction(num, denom);
	}
	
	private static void fillTableIdxKeys(
			KeyTable <IntVector, Integer, Boolean> lookupTable,
			int upTo) {
		List <Integer> list = lookupTable.colKeyList();
		for (int i = 0; i < upTo; ++i) {
			list.add(i);
		}
	}
	
	private static void markInTable(
			KeyTable <IntVector, Integer, Boolean> lookupTable,
			IntVector rowKey,
			long... toMark) {
		for (long colKey : toMark) {
			lookupTable.put(rowKey, (int) colKey, Boolean.TRUE);
		}
	}
	
	private static KeyTable <IntVector, Integer, Boolean> doSolve(
			List <FractionVector> pointList,
			List <IntVector> commonLimits,
			List <IntVector> wishfulBasis) throws Exception {
		
		return solver.solve(pointList, commonLimits, wishfulBasis, /*NullPrintWriter.instance*/new PrintWriter(System.out, true));
	}
}