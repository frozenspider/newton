package org.newtonpolyhedron.solve.polyinter;

import static org.fs.test.Assert.*;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.fs.utils.ArrayUtils;
import org.junit.Test;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.cone.ConeSolverImpl;

public class PolyIntersectionSolverTest {
	
	private PolyIntersectionSolverImpl	solver	= new PolyIntersectionSolverImpl(new ConeSolverImpl());
	
	@SuppressWarnings("unchecked")
	@Test
	public void simpleTest() throws Exception {
		final List <List <FractionVector>> source = Arrays.asList(//
				Arrays.asList(//
						new FractionVector(9, 0, 0),//
						new FractionVector(0, 8, 0),//
						new FractionVector(0, 0, 7),//
						new FractionVector(3, 2, 1) //
				),//
				Arrays.asList(//
						new FractionVector(3, 0, 0),//
						new FractionVector(0, 4, 0),//
						new FractionVector(0, 0, 5),//
						new FractionVector(1, 2, 2) //
				)//
		);
		Map <IntVector, List <List <Integer>>> expected = new LinkedHashMap <IntVector, List <List <Integer>>>();
		expected.put(new IntVector(-4, -3, -6), Arrays.asList(idc(1, 0), idc(1, 1), idc(3, 0), idc(3, 1)));
		expected.put(new IntVector(-4, -3, -3), Arrays.asList(idc(2, 0), idc(2, 1), idc(3, 0), idc(3, 1)));
		expected.put(new IntVector(-1, 0, 0), Arrays.asList(idc(1, 1), idc(1, 2), idc(2, 1), idc(2, 2)));
		expected.put(new IntVector(0, -1, 0), Arrays.asList(idc(0, 0), idc(0, 2), idc(2, 0), idc(2, 2)));
		expected.put(new IntVector(0, 0, -1), Arrays.asList(idc(0, 0), idc(0, 1), idc(1, 0), idc(1, 1)));
		expected.put(new IntVector(8, 9, 5), Arrays.asList(idc(0, 1), idc(0, 3), idc(1, 1), idc(1, 3)));
		expected.put(new IntVector(24, 27, 26), Arrays.asList(idc(0, 2), idc(0, 3), idc(1, 2), idc(1, 3)));
		Map <IntVector, List <List <Integer>>> actual = solver.solve(source, 3);
		
		assertEquals(expected, actual);
	}
	
	// TODO: More tests
	
	private static List <Integer> idc(int... indices) {
		return ArrayUtils.asList(indices);
	}
	
	
	
	@Test
	public void removeNonIntersectingSolutionsTest1() throws Exception {
		List <IntVector> eqSys1 = new ArrayList <IntVector>();
		eqSys1.add(new IntVector(-9, 8, 0));
		eqSys1.add(new IntVector(-9, 0, 7));
		eqSys1.add(new IntVector(-6, 2, 1));
		List <IntVector> eqSys2 = new ArrayList <IntVector>();
		eqSys2.add(new IntVector(-3, 4, 0));
		eqSys2.add(new IntVector(-3, 0, 5));
		eqSys2.add(new IntVector(-2, 2, 2));
		@SuppressWarnings("unchecked")
		List <List <IntVector>> eqSystems = Arrays.<List <IntVector>> asList(eqSys1, eqSys2);
		List <IntVector> solution = new ArrayList <IntVector>(Arrays.asList(//
				new IntVector(-8, -9, -30),//
				new IntVector(-14, -33, -18),//
				new IntVector(0, 0, -1),//
				new IntVector(0, -1, 0),//
				new IntVector(4, 3, 1),//
				new IntVector(5, 2, 3) //
		));
		solver.removeNonIntersectingSolutions(solution, eqSystems);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(0, 0, -1),//
						new IntVector(0, -1, 0) //
				), solution);
	}
	
	@Test
	public void removeNonIntersectingSolutionsTest2() throws Exception {
		List <IntVector> eqSys1 = new ArrayList <IntVector>();
		eqSys1.add(new IntVector(9, -8, 0));
		eqSys1.add(new IntVector(0, -8, 7));
		eqSys1.add(new IntVector(3, -6, 1));
		List <IntVector> eqSys2 = new ArrayList <IntVector>();
		eqSys2.add(new IntVector(3, -4, 0));
		eqSys2.add(new IntVector(0, -4, 5));
		eqSys2.add(new IntVector(1, -2, 2));
		@SuppressWarnings("unchecked")
		List <List <IntVector>> eqSystems = Arrays.<List <IntVector>> asList(eqSys1, eqSys2);
		List <IntVector> solution = new ArrayList <IntVector>(Arrays.asList(//
				new IntVector(-34, -21, -24),//
				new IntVector(0, 0, -1),//
				new IntVector(-4, -3, -6),//
				new IntVector(-1, 0, 0),//
				new IntVector(8, 9, 5),//
				new IntVector(2, 5, 4) //
		));
		solver.removeNonIntersectingSolutions(solution, eqSystems);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(0, 0, -1),//
						new IntVector(-4, -3, -6),//
						new IntVector(-1, 0, 0),//
						new IntVector(8, 9, 5) //
				), solution);
	}
	
	@Test
	public void removeNonIntersectingSolutionsTest3() throws Exception {
		List <IntVector> eqSys1 = new ArrayList <IntVector>();
		eqSys1.add(new IntVector(9, 0, -7));
		eqSys1.add(new IntVector(0, 8, -7));
		eqSys1.add(new IntVector(3, 2, -6));
		List <IntVector> eqSys2 = new ArrayList <IntVector>();
		eqSys2.add(new IntVector(3, 0, -5));
		eqSys2.add(new IntVector(0, 4, -5));
		eqSys2.add(new IntVector(1, 2, -3));
		@SuppressWarnings("unchecked")
		List <List <IntVector>> eqSystems = Arrays.<List <IntVector>> asList(eqSys1, eqSys2);
		List <IntVector> solution = new ArrayList <IntVector>(Arrays.asList(//
				new IntVector(56, 63, 72),//
				new IntVector(0, -1, 0),//
				new IntVector(-1, 0, 0),//
				new IntVector(-20, -15, -12) //
		));
		solver.removeNonIntersectingSolutions(solution, eqSystems);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(0, -1, 0),//
						new IntVector(-1, 0, 0) //
				), solution);
	}
}
