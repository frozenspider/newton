package org.newtonpolyhedron.solve.surface;

import static java.util.Arrays.*;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.fs.utils.collection.CollectionUtils;
import org.fs.utils.collection.list.SortedArrayList;
import org.fs.utils.collection.set.IndexedSet;
import org.fs.utils.collection.table.ArrayListKeyTable;
import org.fs.utils.collection.table.KeyTable;
import org.fs.utils.collection.table.KeyTables;
import org.junit.Test;
import org.newtonpolyhedron.entity.Surface;
import org.newtonpolyhedron.entity.vector.IntVector;

public class SurfaceBuilderImplTest {
	
	private static final SurfaceBuilderImpl	surfaceBuilder	= new SurfaceBuilderImpl();
	
	@Test
	public void mediumTestCase() throws Exception {
		/*-
		   33333==| Q0   Q1   Q2   Q3   Q4  
		   N1 = [ 0 0 -1 ]  |  +    +    +    +    -    
		   N2 = [ -2 -2 1 ] |  +    +    -    -    +    
		   N4 = [ -2 2 1 ]  |  -    -    -    +    +    
		   N3 = [ 2 -2 1 ]  |  -    +    +    -    +    
		   N5 = [ 2 2 1 ]   |  -    -    -    -    + 
		 */
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(0, 0, -1));
		vectors.add(new IntVector(-2, -2, 1));
		vectors.add(new IntVector(-2, 2, 1));
		vectors.add(new IntVector(2, -2, 1));
		vectors.add(new IntVector(2, 2, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 5);
		markInTable(lookupTable, vectors.get(0), 0, 1, 2, 3);
		markInTable(lookupTable, vectors.get(1), 0, 1, 4);
		markInTable(lookupTable, vectors.get(2), 0, 3, 4);
		markInTable(lookupTable, vectors.get(3), 1, 2, 4);
		markInTable(lookupTable, vectors.get(4), 2, 3, 4);
		KeyTables.sortByColHeaders(lookupTable, true);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(0, 1, 2, 3), null));
			borderList2.add(new Surface(asList(0, 1, 4), null));
			borderList2.add(new Surface(asList(0, 3, 4), null));
			borderList2.add(new Surface(asList(1, 2, 4), null));
			borderList2.add(new Surface(asList(2, 3, 4), null));
			expectedSurface.put(2, borderList2);
		}
		
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(0, 1), asListFrom(borderList2, 0, 1)));
			borderList1.add(new Surface(asList(0, 3), asListFrom(borderList2, 0, 2)));
			borderList1.add(new Surface(asList(0, 4), asListFrom(borderList2, 1, 2)));
			borderList1.add(new Surface(asList(1, 2), asListFrom(borderList2, 0, 3)));
			borderList1.add(new Surface(asList(1, 4), asListFrom(borderList2, 1, 3)));
			borderList1.add(new Surface(asList(2, 3), asListFrom(borderList2, 0, 4)));
			borderList1.add(new Surface(asList(2, 4), asListFrom(borderList2, 3, 4)));
			borderList1.add(new Surface(asList(3, 4), asListFrom(borderList2, 2, 4)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(0), asListFrom(borderList1, 0, 1, 2)));
			borderList0.add(new Surface(asList(1), asListFrom(borderList1, 0, 3, 4)));
			borderList0.add(new Surface(asList(2), asListFrom(borderList1, 3, 5, 6)));
			borderList0.add(new Surface(asList(3), asListFrom(borderList1, 1, 5, 7)));
			borderList0.add(new Surface(asList(4), asListFrom(borderList1, 2, 4, 6, 7)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 3));
	}
	
	@Test
	public void largeTestCase() throws Exception {
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
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(-6, -4, 3));
		vectors.add(new IntVector(-4, -6, 3));
		vectors.add(new IntVector(-3, -1, 0));
		vectors.add(new IntVector(-2, -2, 1));
		vectors.add(new IntVector(-2, 2, 1));
		vectors.add(new IntVector(-1, -3, 0));
		vectors.add(new IntVector(-1, -1, -2));
		vectors.add(new IntVector(-1, -1, 1));
		vectors.add(new IntVector(-1, 1, 0));
		vectors.add(new IntVector(0, 0, -1));
		vectors.add(new IntVector(1, -1, 0));
		vectors.add(new IntVector(1, 1, 0));
		vectors.add(new IntVector(2, -2, 1));
		vectors.add(new IntVector(2, 2, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 25);
		markInTable(lookupTable, vectors.get(0), 0, 2, 20, 22);
		markInTable(lookupTable, vectors.get(1), 5, 6, 20, 21);
		markInTable(lookupTable, vectors.get(2), 0, 2, 4);
		markInTable(lookupTable, vectors.get(3), 0, 5, 20);
		markInTable(lookupTable, vectors.get(4), 2, 17, 22);
		markInTable(lookupTable, vectors.get(5), 5, 6, 8);
		markInTable(lookupTable, vectors.get(6), 0, 3, 4, 5, 8, 9);
		markInTable(lookupTable, vectors.get(7), 20, 21, 22);
		markInTable(lookupTable, vectors.get(8), 2, 4, 17, 19);
		markInTable(lookupTable, vectors.get(9), 3, 4, 8, 9, 13, 14, 18, 19);
		markInTable(lookupTable, vectors.get(10), 6, 8, 11, 13);
		markInTable(lookupTable, vectors.get(11), 11, 12, 13, 14, 16, 17, 18, 19);
		markInTable(lookupTable, vectors.get(12), 6, 11, 21);
		markInTable(lookupTable, vectors.get(13), 11, 12, 16, 17, 21, 22);
		KeyTables.sortByColHeaders(lookupTable, true);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(0, 2, 4), null));
			borderList2.add(new Surface(asList(0, 2, 20, 22), null));
			borderList2.add(new Surface(asList(0, 3, 4, 5, 8, 9), null));
			borderList2.add(new Surface(asList(0, 5, 20), null));
			borderList2.add(new Surface(asList(2, 4, 17, 19), null));
			borderList2.add(new Surface(asList(2, 17, 22), null));
			borderList2.add(new Surface(asList(3, 4, 8, 9, 13, 14, 18, 19), null));
			borderList2.add(new Surface(asList(5, 6, 8), null));
			borderList2.add(new Surface(asList(5, 6, 20, 21), null));
			borderList2.add(new Surface(asList(6, 8, 11, 13), null));
			borderList2.add(new Surface(asList(6, 11, 21), null));
			borderList2.add(new Surface(asList(11, 12, 13, 14, 16, 17, 18, 19), null));
			borderList2.add(new Surface(asList(11, 12, 16, 17, 21, 22), null));
			borderList2.add(new Surface(asList(20, 21, 22), null));
			expectedSurface.put(2, borderList2);
		}
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(0, 2), asListFrom(borderList2, 0, 1)));
			borderList1.add(new Surface(asList(0, 4), asListFrom(borderList2, 0, 2)));
			borderList1.add(new Surface(asList(0, 5), asListFrom(borderList2, 2, 3)));
			borderList1.add(new Surface(asList(0, 20), asListFrom(borderList2, 1, 3)));
			borderList1.add(new Surface(asList(2, 4), asListFrom(borderList2, 0, 4)));
			borderList1.add(new Surface(asList(2, 17), asListFrom(borderList2, 4, 5)));
			borderList1.add(new Surface(asList(2, 22), asListFrom(borderList2, 1, 5)));
			borderList1.add(new Surface(asList(3, 4, 8, 9), asListFrom(borderList2, 2, 6)));
			borderList1.add(new Surface(asList(4, 19), asListFrom(borderList2, 4, 6)));
			borderList1.add(new Surface(asList(5, 6), asListFrom(borderList2, 7, 8)));
			borderList1.add(new Surface(asList(5, 8), asListFrom(borderList2, 2, 7)));
			borderList1.add(new Surface(asList(5, 20), asListFrom(borderList2, 3, 8)));
			borderList1.add(new Surface(asList(6, 8), asListFrom(borderList2, 7, 9)));
			borderList1.add(new Surface(asList(6, 11), asListFrom(borderList2, 9, 10)));
			borderList1.add(new Surface(asList(6, 21), asListFrom(borderList2, 8, 10)));
			borderList1.add(new Surface(asList(8, 13), asListFrom(borderList2, 6, 9)));
			borderList1.add(new Surface(asList(11, 12, 16, 17), asListFrom(borderList2, 11, 12)));
			borderList1.add(new Surface(asList(11, 13), asListFrom(borderList2, 9, 11)));
			borderList1.add(new Surface(asList(11, 21), asListFrom(borderList2, 10, 12)));
			borderList1.add(new Surface(asList(13, 14, 18, 19), asListFrom(borderList2, 6, 11)));
			borderList1.add(new Surface(asList(17, 19), asListFrom(borderList2, 4, 11)));
			borderList1.add(new Surface(asList(17, 22), asListFrom(borderList2, 5, 12)));
			borderList1.add(new Surface(asList(20, 21), asListFrom(borderList2, 8, 13)));
			borderList1.add(new Surface(asList(20, 22), asListFrom(borderList2, 1, 13)));
			borderList1.add(new Surface(asList(21, 22), asListFrom(borderList2, 12, 13)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(0), asListFrom(borderList1, 0, 1, 2, 3)));
			borderList0.add(new Surface(asList(2), asListFrom(borderList1, 0, 4, 5, 6)));
			borderList0.add(new Surface(asList(4), asListFrom(borderList1, 1, 4, 7, 8)));
			borderList0.add(new Surface(asList(5), asListFrom(borderList1, 2, 9, 10, 11)));
			borderList0.add(new Surface(asList(6), asListFrom(borderList1, 9, 12, 13, 14)));
			borderList0.add(new Surface(asList(8), asListFrom(borderList1, 7, 10, 12, 15)));
			borderList0.add(new Surface(asList(11), asListFrom(borderList1, 13, 16, 17, 18)));
			borderList0.add(new Surface(asList(13), asListFrom(borderList1, 15, 17, 19)));
			borderList0.add(new Surface(asList(17), asListFrom(borderList1, 5, 16, 20, 21)));
			borderList0.add(new Surface(asList(19), asListFrom(borderList1, 8, 19, 20)));
			borderList0.add(new Surface(asList(20), asListFrom(borderList1, 3, 11, 22, 23)));
			borderList0.add(new Surface(asList(21), asListFrom(borderList1, 14, 18, 22, 24)));
			borderList0.add(new Surface(asList(22), asListFrom(borderList1, 6, 21, 23, 24)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 3));
	}
	
	@Test
	public void penleve1() throws Exception {
		/*-
		3 Normal vectors: 3
		3333==| Q0   Q1   Q2   Q3   Q4   Q5   Q6   Q7   Q8   Q9  
		N1 = [ -1 0 ] |  +    +    +    +    +    +    -    -    -    -    
		N2 = [ 1 -1 ] |  -    -    -    -    -    +    -    -    -    +    
		N3 = [ 1 0 ]  |  -    -    -    -    -    -    -    -    +    +    
		N4 = [ 1 1 ]  |  -    -    +    -    -    -    -    -    +    -
		 */
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(-1, 0));
		vectors.add(new IntVector(1, -1));
		vectors.add(new IntVector(1, 0));
		vectors.add(new IntVector(1, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 9);
		markInTable(lookupTable, vectors.get(0), 0, 1, 2, 3, 4, 5);
		markInTable(lookupTable, vectors.get(1), 5, 9);
		markInTable(lookupTable, vectors.get(2), 8, 9);
		markInTable(lookupTable, vectors.get(3), 2, 8);
		KeyTables.sortByColHeaders(lookupTable, true);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(0, 1, 2, 3, 4, 5), null));
			borderList1.add(new Surface(asList(2, 8), null));
			borderList1.add(new Surface(asList(5, 9), null));
			borderList1.add(new Surface(asList(8, 9), null));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(2), asListFrom(borderList1, 0, 1)));
			borderList0.add(new Surface(asList(5), asListFrom(borderList1, 0, 2)));
			borderList0.add(new Surface(asList(8), asListFrom(borderList1, 1, 3)));
			borderList0.add(new Surface(asList(9), asListFrom(borderList1, 2, 3)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 2));
	}
	
	@Test
	public void halfCubeDiag() throws Exception {
		/*-
		33333=| Q0   Q1   Q2   Q3   Q4   Q5  
		N1 = [ 0 -1 1 ] |  +    +    -    +    -    +    
		N4 = [ 0 0 -1 ] |  +    +    +    -    +    -    
		N2 = [ -1 0 0 ] |  +    -    +    +    -    -    
		N3 = [ 0 1 0  ] |  -    -    +    +    +    +    
		N5 = [ 1 0 0  ] |  -    +    -    -    +    +  
		 */
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(0, -1, 1));
		vectors.add(new IntVector(0, 0, -1));
		vectors.add(new IntVector(-1, 0, 0));
		vectors.add(new IntVector(0, 1, 0));
		vectors.add(new IntVector(1, 0, 0));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 5);
		markInTable(lookupTable, vectors.get(0), 0, 1, 3, 5);
		markInTable(lookupTable, vectors.get(1), 0, 1, 2, 4);
		markInTable(lookupTable, vectors.get(2), 0, 2, 3);
		markInTable(lookupTable, vectors.get(3), 2, 3, 4, 5);
		markInTable(lookupTable, vectors.get(4), 1, 4, 5);
		KeyTables.sortByColHeaders(lookupTable, true);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(0, 1, 2, 4), null));
			borderList2.add(new Surface(asList(0, 1, 3, 5), null));
			borderList2.add(new Surface(asList(0, 2, 3), null));
			borderList2.add(new Surface(asList(1, 4, 5), null));
			borderList2.add(new Surface(asList(2, 3, 4, 5), null));
			expectedSurface.put(2, borderList2);
		}
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(0, 1), asListFrom(borderList2, 0, 1)));
			borderList1.add(new Surface(asList(0, 2), asListFrom(borderList2, 0, 2)));
			borderList1.add(new Surface(asList(0, 3), asListFrom(borderList2, 1, 2)));
			borderList1.add(new Surface(asList(1, 4), asListFrom(borderList2, 0, 3)));
			borderList1.add(new Surface(asList(1, 5), asListFrom(borderList2, 1, 3)));
			borderList1.add(new Surface(asList(2, 3), asListFrom(borderList2, 2, 4)));
			borderList1.add(new Surface(asList(2, 4), asListFrom(borderList2, 0, 4)));
			borderList1.add(new Surface(asList(3, 5), asListFrom(borderList2, 1, 4)));
			borderList1.add(new Surface(asList(4, 5), asListFrom(borderList2, 3, 4)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(0), asListFrom(borderList1, 0, 1, 2)));
			borderList0.add(new Surface(asList(1), asListFrom(borderList1, 0, 3, 4)));
			borderList0.add(new Surface(asList(2), asListFrom(borderList1, 1, 5, 6)));
			borderList0.add(new Surface(asList(3), asListFrom(borderList1, 2, 5, 7)));
			borderList0.add(new Surface(asList(4), asListFrom(borderList1, 3, 6, 8)));
			borderList0.add(new Surface(asList(5), asListFrom(borderList1, 4, 7, 8)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 3));
	}
	
	@Test
	public void brunoPage19Through30() throws Exception {
		/*-
		                     | Q0  Q1  Q2  Q3  Q4
		   N1 = [ -2 -1 -1 ] |  +   -   +   +   -
		   N2 = [ -1 -2 -1 ] |  +   +   -   +   +
		   N3 = [ -1 -1 -2 ] |  +   +   +   -   -
		   N4 = [  1  1  1 ] |  -   +   +   +   +
		 */
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(-2, -1, -1));
		vectors.add(new IntVector(-1, -2, -1));
		vectors.add(new IntVector(-1, -1, -2));
		vectors.add(new IntVector(1, 1, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 5);
		markInTable(lookupTable, vectors.get(0), 0, 2, 3);
		markInTable(lookupTable, vectors.get(1), 0, 1, 3, 4);
		markInTable(lookupTable, vectors.get(2), 0, 1, 2);
		markInTable(lookupTable, vectors.get(3), 1, 2, 3, 4);
		KeyTables.sortByColHeaders(lookupTable, true);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(0, 1, 2), null));
			borderList2.add(new Surface(asList(0, 1, 3, 4), null));
			borderList2.add(new Surface(asList(0, 2, 3), null));
			borderList2.add(new Surface(asList(1, 2, 3, 4), null));
			expectedSurface.put(2, borderList2);
		}
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(0, 1), asListFrom(borderList2, 0, 1)));
			borderList1.add(new Surface(asList(0, 2), asListFrom(borderList2, 0, 2)));
			borderList1.add(new Surface(asList(0, 3), asListFrom(borderList2, 1, 2)));
			borderList1.add(new Surface(asList(1, 2), asListFrom(borderList2, 0, 3)));
			borderList1.add(new Surface(asList(1, 3, 4), asListFrom(borderList2, 1, 3)));
			borderList1.add(new Surface(asList(2, 3), asListFrom(borderList2, 2, 3)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(0), asListFrom(borderList1, 0, 1, 2)));
			borderList0.add(new Surface(asList(1), asListFrom(borderList1, 0, 3, 4)));
			borderList0.add(new Surface(asList(2), asListFrom(borderList1, 1, 3, 5)));
			borderList0.add(new Surface(asList(3), asListFrom(borderList1, 2, 4, 5)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 3));
	}
	
	@Test
	public void brunoEx1Page18() throws Exception {
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
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(-1, -1, -1, -1));
		vectors.add(new IntVector(-1, -1, -1, 0));
		vectors.add(new IntVector(-1, -1, 0, -1));
		vectors.add(new IntVector(-1, -1, 0, 0));
		vectors.add(new IntVector(-1, 0, -1, -1));
		vectors.add(new IntVector(-1, 0, -1, 0));
		vectors.add(new IntVector(-1, 0, 0, -1));
		vectors.add(new IntVector(-1, 0, 0, 0));
		vectors.add(new IntVector(0, -1, -1, -1));
		vectors.add(new IntVector(0, -1, -1, 0));
		vectors.add(new IntVector(0, -1, 0, -1));
		vectors.add(new IntVector(0, -1, 0, 0));
		vectors.add(new IntVector(0, 0, -1, -1));
		vectors.add(new IntVector(0, 0, -1, 0));
		vectors.add(new IntVector(0, 0, 0, -1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 12);
		markInTable(lookupTable, vectors.get(0), 0, 1, 2, 8, 9, 10, 11);
		markInTable(lookupTable, vectors.get(1), 8, 9, 10);
		markInTable(lookupTable, vectors.get(2), 8, 9, 11);
		markInTable(lookupTable, vectors.get(3), 8, 9);
		markInTable(lookupTable, vectors.get(4), 2, 8, 10, 11);
		markInTable(lookupTable, vectors.get(5), 8, 10);
		markInTable(lookupTable, vectors.get(6), 8, 11);
		markInTable(lookupTable, vectors.get(7), 8);
		markInTable(lookupTable, vectors.get(8), 1, 2, 9, 10, 11);
		markInTable(lookupTable, vectors.get(9), 9, 10);
		markInTable(lookupTable, vectors.get(10), 9, 11);
		markInTable(lookupTable, vectors.get(11), 9);
		markInTable(lookupTable, vectors.get(12), 2, 10, 11);
		markInTable(lookupTable, vectors.get(13), 10);
		markInTable(lookupTable, vectors.get(14), 11);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 3
		IndexedSet <Surface> borderList3 = new SortedArrayList <Surface>();
		{
			borderList3.add(new Surface(asList(0, 1, 2, 8, 9, 10, 11), null));
			expectedSurface.put(3, borderList3);
		}
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(1, 2, 9, 10, 11), asListFrom(borderList3, 0)));
			borderList2.add(new Surface(asList(2, 8, 10, 11), asListFrom(borderList3, 0)));
			borderList2.add(new Surface(asList(8, 9, 10), asListFrom(borderList3, 0)));
			borderList2.add(new Surface(asList(8, 9, 11), asListFrom(borderList3, 0)));
			expectedSurface.put(2, borderList2);
		}
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(2, 10, 11), asListFrom(borderList2, 0, 1)));
			borderList1.add(new Surface(asList(8, 9), asListFrom(borderList2, 2, 3)));
			borderList1.add(new Surface(asList(8, 10), asListFrom(borderList2, 1, 2)));
			borderList1.add(new Surface(asList(8, 11), asListFrom(borderList2, 1, 3)));
			borderList1.add(new Surface(asList(9, 10), asListFrom(borderList2, 0, 2)));
			borderList1.add(new Surface(asList(9, 11), asListFrom(borderList2, 0, 3)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(8), asListFrom(borderList1, 1, 2, 3)));
			borderList0.add(new Surface(asList(9), asListFrom(borderList1, 1, 4, 5)));
			borderList0.add(new Surface(asList(10), asListFrom(borderList1, 0, 2, 4)));
			borderList0.add(new Surface(asList(11), asListFrom(borderList1, 0, 3, 5)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 4));
	}
	
	@Test
	public void brunoEx2Page35() throws Exception {
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
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(-1, -1, -1, -1));
		vectors.add(new IntVector(-1, -1, -1, 1));
		vectors.add(new IntVector(-1, -1, 1, -1));
		vectors.add(new IntVector(-1, -1, 1, 1));
		vectors.add(new IntVector(-1, 1, -1, -1));
		vectors.add(new IntVector(-1, 1, -1, 1));
		vectors.add(new IntVector(-1, 1, 1, -1));
		vectors.add(new IntVector(-1, 1, 1, 1));
		vectors.add(new IntVector(1, -1, -1, -1));
		vectors.add(new IntVector(1, -1, -1, 1));
		vectors.add(new IntVector(1, -1, 1, -1));
		vectors.add(new IntVector(1, -1, 1, 1));
		vectors.add(new IntVector(1, 1, -1, -1));
		vectors.add(new IntVector(1, 1, -1, 1));
		vectors.add(new IntVector(1, 1, 1, -1));
		vectors.add(new IntVector(1, 1, 1, 1));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 12);
		markInTable(lookupTable, vectors.get(0), 0, 1, 2, 8, 9, 10, 11);
		markInTable(lookupTable, vectors.get(1), 7, 8, 9, 10);
		markInTable(lookupTable, vectors.get(2), 6, 8, 9, 11);
		markInTable(lookupTable, vectors.get(3), 6, 7, 8, 9);
		markInTable(lookupTable, vectors.get(4), 2, 5, 8, 10, 11);
		markInTable(lookupTable, vectors.get(5), 5, 7, 8, 10);
		markInTable(lookupTable, vectors.get(6), 5, 6, 8, 11);
		markInTable(lookupTable, vectors.get(7), 5, 6, 7, 8);
		markInTable(lookupTable, vectors.get(8), 1, 2, 4, 9, 10, 11);
		markInTable(lookupTable, vectors.get(9), 4, 7, 9, 10);
		markInTable(lookupTable, vectors.get(10), 4, 6, 9, 11);
		markInTable(lookupTable, vectors.get(11), 4, 6, 7, 9);
		markInTable(lookupTable, vectors.get(12), 2, 4, 5, 10, 11);
		markInTable(lookupTable, vectors.get(13), 4, 5, 7, 10);
		markInTable(lookupTable, vectors.get(14), 4, 5, 6, 11);
		markInTable(lookupTable, vectors.get(15), 4, 5, 6, 7);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 3
		IndexedSet <Surface> borderList3 = new SortedArrayList <Surface>();
		{
			borderList3.add(new Surface(asList(0, 1, 2, 8, 9, 10, 11), null));
			borderList3.add(new Surface(asList(1, 2, 4, 9, 10, 11), null));
			borderList3.add(new Surface(asList(2, 4, 5, 10, 11), null));
			borderList3.add(new Surface(asList(2, 5, 8, 10, 11), null));
			borderList3.add(new Surface(asList(4, 5, 6, 7), null));
			borderList3.add(new Surface(asList(4, 5, 6, 11), null));
			borderList3.add(new Surface(asList(4, 5, 7, 10), null));
			borderList3.add(new Surface(asList(4, 6, 7, 9), null));
			borderList3.add(new Surface(asList(4, 6, 9, 11), null));
			borderList3.add(new Surface(asList(4, 7, 9, 10), null));
			borderList3.add(new Surface(asList(5, 6, 7, 8), null));
			borderList3.add(new Surface(asList(5, 6, 8, 11), null));
			borderList3.add(new Surface(asList(5, 7, 8, 10), null));
			borderList3.add(new Surface(asList(6, 7, 8, 9), null));
			borderList3.add(new Surface(asList(6, 8, 9, 11), null));
			borderList3.add(new Surface(asList(7, 8, 9, 10), null));
			expectedSurface.put(3, borderList3);
		}
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(1, 2, 9, 10, 11), asListFrom(borderList3, 0, 1)));
			borderList2.add(new Surface(asList(2, 4, 10, 11), asListFrom(borderList3, 1, 2)));
			borderList2.add(new Surface(asList(2, 5, 10, 11), asListFrom(borderList3, 2, 3)));
			borderList2.add(new Surface(asList(2, 8, 10, 11), asListFrom(borderList3, 0, 3)));
			borderList2.add(new Surface(asList(4, 5, 6), asListFrom(borderList3, 4, 5)));
			borderList2.add(new Surface(asList(4, 5, 7), asListFrom(borderList3, 4, 6)));
			borderList2.add(new Surface(asList(4, 5, 10), asListFrom(borderList3, 2, 6)));
			borderList2.add(new Surface(asList(4, 5, 11), asListFrom(borderList3, 2, 5)));
			borderList2.add(new Surface(asList(4, 6, 7), asListFrom(borderList3, 4, 7)));
			borderList2.add(new Surface(asList(4, 6, 9), asListFrom(borderList3, 7, 8)));
			borderList2.add(new Surface(asList(4, 6, 11), asListFrom(borderList3, 5, 8)));
			borderList2.add(new Surface(asList(4, 7, 9), asListFrom(borderList3, 7, 9)));
			borderList2.add(new Surface(asList(4, 7, 10), asListFrom(borderList3, 6, 9)));
			borderList2.add(new Surface(asList(4, 9, 10), asListFrom(borderList3, 1, 9)));
			borderList2.add(new Surface(asList(4, 9, 11), asListFrom(borderList3, 1, 8)));
			borderList2.add(new Surface(asList(5, 6, 7), asListFrom(borderList3, 4, 10)));
			borderList2.add(new Surface(asList(5, 6, 8), asListFrom(borderList3, 10, 11)));
			borderList2.add(new Surface(asList(5, 6, 11), asListFrom(borderList3, 5, 11)));
			borderList2.add(new Surface(asList(5, 7, 8), asListFrom(borderList3, 10, 12)));
			borderList2.add(new Surface(asList(5, 7, 10), asListFrom(borderList3, 6, 12)));
			borderList2.add(new Surface(asList(5, 8, 10), asListFrom(borderList3, 3, 12)));
			borderList2.add(new Surface(asList(5, 8, 11), asListFrom(borderList3, 3, 11)));
			borderList2.add(new Surface(asList(6, 7, 8), asListFrom(borderList3, 10, 13)));
			borderList2.add(new Surface(asList(6, 7, 9), asListFrom(borderList3, 7, 13)));
			borderList2.add(new Surface(asList(6, 8, 9), asListFrom(borderList3, 13, 14)));
			borderList2.add(new Surface(asList(6, 8, 11), asListFrom(borderList3, 11, 14)));
			borderList2.add(new Surface(asList(6, 9, 11), asListFrom(borderList3, 8, 14)));
			borderList2.add(new Surface(asList(7, 8, 9), asListFrom(borderList3, 13, 15)));
			borderList2.add(new Surface(asList(7, 8, 10), asListFrom(borderList3, 12, 15)));
			borderList2.add(new Surface(asList(7, 9, 10), asListFrom(borderList3, 9, 15)));
			borderList2.add(new Surface(asList(8, 9, 10), asListFrom(borderList3, 0, 15)));
			borderList2.add(new Surface(asList(8, 9, 11), asListFrom(borderList3, 0, 14)));
			expectedSurface.put(2, borderList2);
		}
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(2, 10, 11), asListFrom(borderList2, 0, 1, 2, 3)));
			borderList1.add(new Surface(asList(4, 5), asListFrom(borderList2, 4, 5, 6, 7)));
			borderList1.add(new Surface(asList(4, 6), asListFrom(borderList2, 4, 8, 9, 10)));
			borderList1.add(new Surface(asList(4, 7), asListFrom(borderList2, 5, 8, 11, 12)));
			borderList1.add(new Surface(asList(4, 9), asListFrom(borderList2, 9, 11, 13, 14)));
			borderList1.add(new Surface(asList(4, 10), asListFrom(borderList2, 1, 6, 12, 13)));
			borderList1.add(new Surface(asList(4, 11), asListFrom(borderList2, 1, 7, 10, 14)));
			borderList1.add(new Surface(asList(5, 6), asListFrom(borderList2, 4, 15, 16, 17)));
			borderList1.add(new Surface(asList(5, 7), asListFrom(borderList2, 5, 15, 18, 19)));
			borderList1.add(new Surface(asList(5, 8), asListFrom(borderList2, 16, 18, 20, 21)));
			borderList1.add(new Surface(asList(5, 10), asListFrom(borderList2, 2, 6, 19, 20)));
			borderList1.add(new Surface(asList(5, 11), asListFrom(borderList2, 2, 7, 17, 21)));
			borderList1.add(new Surface(asList(6, 7), asListFrom(borderList2, 8, 15, 22, 23)));
			borderList1.add(new Surface(asList(6, 8), asListFrom(borderList2, 16, 22, 24, 25)));
			borderList1.add(new Surface(asList(6, 9), asListFrom(borderList2, 9, 23, 24, 26)));
			borderList1.add(new Surface(asList(6, 11), asListFrom(borderList2, 10, 17, 25, 26)));
			borderList1.add(new Surface(asList(7, 8), asListFrom(borderList2, 18, 22, 27, 28)));
			borderList1.add(new Surface(asList(7, 9), asListFrom(borderList2, 11, 23, 27, 29)));
			borderList1.add(new Surface(asList(7, 10), asListFrom(borderList2, 12, 19, 28, 29)));
			borderList1.add(new Surface(asList(8, 9), asListFrom(borderList2, 24, 27, 30, 31)));
			borderList1.add(new Surface(asList(8, 10), asListFrom(borderList2, 3, 20, 28, 30)));
			borderList1.add(new Surface(asList(8, 11), asListFrom(borderList2, 3, 21, 25, 31)));
			borderList1.add(new Surface(asList(9, 10), asListFrom(borderList2, 0, 13, 29, 30)));
			borderList1.add(new Surface(asList(9, 11), asListFrom(borderList2, 0, 14, 26, 31)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(4), asListFrom(borderList1, 1, 2, 3, 4, 5, 6)));
			borderList0.add(new Surface(asList(5), asListFrom(borderList1, 1, 7, 8, 9, 10, 11)));
			borderList0.add(new Surface(asList(6), asListFrom(borderList1, 2, 7, 12, 13, 14, 15)));
			borderList0.add(new Surface(asList(7), asListFrom(borderList1, 3, 8, 12, 16, 17, 18)));
			borderList0.add(new Surface(asList(8), asListFrom(borderList1, 9, 13, 16, 19, 20, 21)));
			borderList0.add(new Surface(asList(9), asListFrom(borderList1, 4, 14, 17, 19, 22, 23)));
			borderList0.add(new Surface(asList(10), asListFrom(borderList1, 0, 5, 10, 18, 20, 22)));
			borderList0.add(new Surface(asList(11), asListFrom(borderList1, 0, 6, 11, 15, 21, 23)));
			expectedSurface.put(0, borderList0);
		}
		assertEquals(expectedSurface, surfaceBuilder.getSurfaces(lookupTable, 4));
	}
	
	@Test
	public void unknownBrunoEx() throws Exception {
		// Some random vectors - I haven't found actual ones
		List <IntVector> vectors = new ArrayList <IntVector>();
		vectors.add(new IntVector(-1, 0, 0, 0));
		vectors.add(new IntVector(0, -1, 0, 0));
		vectors.add(new IntVector(0, 0, -1, 0));
		vectors.add(new IntVector(0, 0, 0, -1));
		vectors.add(new IntVector(0, 0, 0, 1));
		vectors.add(new IntVector(0, 0, 1, 0));
		vectors.add(new IntVector(0, 1, 0, 0));
		vectors.add(new IntVector(1, 0, 0, 0));
		
		KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, 20);
		markInTable(lookupTable, vectors.get(0), 1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19);
		markInTable(lookupTable, vectors.get(1), 2, 3, 6, 7, 10, 11, 14, 15, 18, 19);
		markInTable(lookupTable, vectors.get(2), 4, 5, 6, 7, 8, 9, 10, 11);
		markInTable(lookupTable, vectors.get(3), 3, 5, 7, 9, 11, 13, 15, 17, 19);
		markInTable(lookupTable, vectors.get(4), 4, 5, 6, 7, 12, 13, 14, 15);
		markInTable(lookupTable, vectors.get(5), 4, 5, 8, 9, 12, 13, 16, 17);
		markInTable(lookupTable, vectors.get(6), 4, 6, 8, 10, 12, 14, 16, 18);
		markInTable(lookupTable, vectors.get(7), 12, 13, 14, 15, 16, 17, 18, 19);
		
		Map <Integer, IndexedSet <Surface>> expectedSurface = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		
		// Dimension: 3
		IndexedSet <Surface> borderList3 = new SortedArrayList <Surface>();
		{
			borderList3.add(new Surface(asList(1, 2, 3, 8, 9, 10, 11, 16, 17, 18, 19), null));
			borderList3.add(new Surface(asList(2, 3, 6, 7, 10, 11, 14, 15, 18, 19), null));
			borderList3.add(new Surface(asList(3, 5, 7, 9, 11, 13, 15, 17, 19), null));
			borderList3.add(new Surface(asList(4, 5, 6, 7, 8, 9, 10, 11), null));
			borderList3.add(new Surface(asList(4, 5, 6, 7, 12, 13, 14, 15), null));
			borderList3.add(new Surface(asList(4, 5, 8, 9, 12, 13, 16, 17), null));
			borderList3.add(new Surface(asList(4, 6, 8, 10, 12, 14, 16, 18), null));
			borderList3.add(new Surface(asList(12, 13, 14, 15, 16, 17, 18, 19), null));
			expectedSurface.put(3, borderList3);
		}
		// Dimension: 2
		IndexedSet <Surface> borderList2 = new SortedArrayList <Surface>();
		{
			borderList2.add(new Surface(asList(2, 3, 10, 11, 18, 19), asListFrom(borderList3, 0, 1)));
			borderList2.add(new Surface(asList(3, 7, 11, 15, 19), asListFrom(borderList3, 1, 2)));
			borderList2.add(new Surface(asList(3, 9, 11, 17, 19), asListFrom(borderList3, 0, 2)));
			borderList2.add(new Surface(asList(4, 5, 6, 7), asListFrom(borderList3, 3, 4)));
			borderList2.add(new Surface(asList(4, 5, 8, 9), asListFrom(borderList3, 3, 5)));
			borderList2.add(new Surface(asList(4, 5, 12, 13), asListFrom(borderList3, 4, 5)));
			borderList2.add(new Surface(asList(4, 6, 8, 10), asListFrom(borderList3, 3, 6)));
			borderList2.add(new Surface(asList(4, 6, 12, 14), asListFrom(borderList3, 4, 6)));
			borderList2.add(new Surface(asList(4, 8, 12, 16), asListFrom(borderList3, 5, 6)));
			borderList2.add(new Surface(asList(5, 7, 9, 11), asListFrom(borderList3, 2, 3)));
			borderList2.add(new Surface(asList(5, 7, 13, 15), asListFrom(borderList3, 2, 4)));
			borderList2.add(new Surface(asList(5, 9, 13, 17), asListFrom(borderList3, 2, 5)));
			borderList2.add(new Surface(asList(6, 7, 10, 11), asListFrom(borderList3, 1, 3)));
			borderList2.add(new Surface(asList(6, 7, 14, 15), asListFrom(borderList3, 1, 4)));
			borderList2.add(new Surface(asList(6, 10, 14, 18), asListFrom(borderList3, 1, 6)));
			borderList2.add(new Surface(asList(8, 9, 10, 11), asListFrom(borderList3, 0, 3)));
			borderList2.add(new Surface(asList(8, 9, 16, 17), asListFrom(borderList3, 0, 5)));
			borderList2.add(new Surface(asList(8, 10, 16, 18), asListFrom(borderList3, 0, 6)));
			borderList2.add(new Surface(asList(12, 13, 14, 15), asListFrom(borderList3, 4, 7)));
			borderList2.add(new Surface(asList(12, 13, 16, 17), asListFrom(borderList3, 5, 7)));
			borderList2.add(new Surface(asList(12, 14, 16, 18), asListFrom(borderList3, 6, 7)));
			borderList2.add(new Surface(asList(14, 15, 18, 19), asListFrom(borderList3, 1, 7)));
			borderList2.add(new Surface(asList(16, 17, 18, 19), asListFrom(borderList3, 0, 7)));
			borderList2.add(new Surface(asList(13, 15, 17, 19), asListFrom(borderList3, 2, 7)));
			expectedSurface.put(2, borderList2);
		}
		// Dimension: 1
		IndexedSet <Surface> borderList1 = new SortedArrayList <Surface>();
		{
			borderList1.add(new Surface(asList(3, 11, 19), asListFrom(borderList2, 0, 1, 2)));
			borderList1.add(new Surface(asList(4, 5), asListFrom(borderList2, 3, 4, 5)));
			borderList1.add(new Surface(asList(4, 6), asListFrom(borderList2, 3, 6, 7)));
			borderList1.add(new Surface(asList(4, 8), asListFrom(borderList2, 4, 6, 8)));
			borderList1.add(new Surface(asList(4, 12), asListFrom(borderList2, 5, 7, 8)));
			borderList1.add(new Surface(asList(5, 7), asListFrom(borderList2, 3, 9, 10)));
			borderList1.add(new Surface(asList(5, 9), asListFrom(borderList2, 4, 9, 11)));
			borderList1.add(new Surface(asList(5, 13), asListFrom(borderList2, 5, 10, 11)));
			borderList1.add(new Surface(asList(6, 7), asListFrom(borderList2, 3, 12, 13)));
			borderList1.add(new Surface(asList(6, 10), asListFrom(borderList2, 6, 12, 14)));
			borderList1.add(new Surface(asList(6, 14), asListFrom(borderList2, 7, 13, 14)));
			borderList1.add(new Surface(asList(7, 11), asListFrom(borderList2, 1, 9, 12)));
			borderList1.add(new Surface(asList(7, 15), asListFrom(borderList2, 1, 10, 13)));
			borderList1.add(new Surface(asList(8, 9), asListFrom(borderList2, 4, 15, 16)));
			borderList1.add(new Surface(asList(8, 10), asListFrom(borderList2, 6, 15, 17)));
			borderList1.add(new Surface(asList(8, 16), asListFrom(borderList2, 8, 16, 17)));
			borderList1.add(new Surface(asList(9, 11), asListFrom(borderList2, 2, 9, 15)));
			borderList1.add(new Surface(asList(9, 17), asListFrom(borderList2, 2, 11, 16)));
			borderList1.add(new Surface(asList(10, 11), asListFrom(borderList2, 0, 12, 15)));
			borderList1.add(new Surface(asList(10, 18), asListFrom(borderList2, 0, 14, 17)));
			borderList1.add(new Surface(asList(12, 13), asListFrom(borderList2, 5, 18, 19)));
			borderList1.add(new Surface(asList(12, 14), asListFrom(borderList2, 7, 18, 20)));
			borderList1.add(new Surface(asList(12, 16), asListFrom(borderList2, 8, 19, 20)));
			borderList1.add(new Surface(asList(13, 15), asListFrom(borderList2, 10, 18, 21)));
			borderList1.add(new Surface(asList(13, 17), asListFrom(borderList2, 11, 19, 21)));
			borderList1.add(new Surface(asList(14, 15), asListFrom(borderList2, 13, 18, 22)));
			borderList1.add(new Surface(asList(14, 18), asListFrom(borderList2, 14, 20, 22)));
			borderList1.add(new Surface(asList(15, 19), asListFrom(borderList2, 1, 21, 22)));
			borderList1.add(new Surface(asList(16, 17), asListFrom(borderList2, 16, 19, 23)));
			borderList1.add(new Surface(asList(16, 18), asListFrom(borderList2, 17, 20, 23)));
			borderList1.add(new Surface(asList(17, 19), asListFrom(borderList2, 2, 21, 23)));
			borderList1.add(new Surface(asList(18, 19), asListFrom(borderList2, 0, 22, 23)));
			expectedSurface.put(1, borderList1);
		}
		// Dimension: 0
		IndexedSet <Surface> borderList0 = new SortedArrayList <Surface>();
		{
			borderList0.add(new Surface(asList(4), asListFrom(borderList1, 1, 2, 3, 4)));
			borderList0.add(new Surface(asList(5), asListFrom(borderList1, 1, 5, 6, 7)));
			borderList0.add(new Surface(asList(6), asListFrom(borderList1, 2, 8, 9, 10)));
			borderList0.add(new Surface(asList(7), asListFrom(borderList1, 5, 8, 11, 12)));
			borderList0.add(new Surface(asList(8), asListFrom(borderList1, 3, 13, 14, 15)));
			borderList0.add(new Surface(asList(9), asListFrom(borderList1, 6, 13, 16, 17)));
			borderList0.add(new Surface(asList(10), asListFrom(borderList1, 9, 14, 18, 19)));
			borderList0.add(new Surface(asList(11), asListFrom(borderList1, 0, 11, 16, 18)));
			borderList0.add(new Surface(asList(12), asListFrom(borderList1, 4, 20, 21, 22)));
			borderList0.add(new Surface(asList(13), asListFrom(borderList1, 7, 20, 23, 24)));
			borderList0.add(new Surface(asList(14), asListFrom(borderList1, 10, 21, 25, 26)));
			borderList0.add(new Surface(asList(15), asListFrom(borderList1, 12, 23, 25, 27)));
			borderList0.add(new Surface(asList(16), asListFrom(borderList1, 15, 22, 28, 29)));
			borderList0.add(new Surface(asList(17), asListFrom(borderList1, 17, 24, 28, 30)));
			borderList0.add(new Surface(asList(18), asListFrom(borderList1, 19, 26, 29, 31)));
			borderList0.add(new Surface(asList(19), asListFrom(borderList1, 0, 27, 30, 31)));
			expectedSurface.put(0, borderList0);
		}
		Map <Integer, IndexedSet <Surface>> actualSurface = surfaceBuilder.getSurfaces(lookupTable, 4);
		assertEquals(expectedSurface, actualSurface);
	}
	
	@Test
	public void findCommonSurfacesFailTests() throws Exception {
		IndexedSet <Surface> surfaces = CollectionUtils.emptySet();
		List <List <Integer>> lookupTableData = Collections.emptyList();
		
		// Poly dimension < 2
		try {
			surfaceBuilder.findCommonSurfacesJava(1, 1, surfaces, lookupTableData);
			fail();
		} catch(IllegalArgumentException ex) {/* Just as planned */}
		
		// Negative target dimension
		try {
			surfaceBuilder.findCommonSurfacesJava(3, -1, surfaces, lookupTableData);
			fail();
		} catch(IllegalArgumentException ex) {/* Just as planned */}
		
		// Target dimension > Poly dimension
		try {
			surfaceBuilder.findCommonSurfacesJava(3, 4, surfaces, lookupTableData);
			fail();
		} catch(IllegalArgumentException ex) {/* Just as planned */}
		
		// Target dimension = Poly dimension
		try {
			surfaceBuilder.findCommonSurfacesJava(3, 3, surfaces, lookupTableData);
			fail();
		} catch(IllegalArgumentException ex) {/* Just as planned */}
		
		// Target dimension < Poly dimension
		surfaceBuilder.findCommonSurfacesJava(3, 2, surfaces, lookupTableData);
	}
	
	//
	// Supportive
	//
	private static void fillTableIdxKeys(KeyTable <IntVector, Integer, Boolean> lookupTable, int upTo) {
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
	
	private static <T>List <T> asListFrom(IndexedSet <T> indexedSet, int... indices) {
		List <T> result = new ArrayList <T>(indexedSet.size());
		for (int i : indices) {
			result.add(indexedSet.get(i));
		}
		return result;
	}
}