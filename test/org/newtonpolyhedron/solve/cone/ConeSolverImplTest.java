package org.newtonpolyhedron.solve.cone;

import static org.fs.test.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.cone.ConeSolver;
import org.newtonpolyhedron.solve.cone.ConeSolverImpl;
import org.newtonpolyhedron.utils.NullPrintWriter;

public class ConeSolverImplTest {
	
	private static final ConeSolver	coneSolver	= new ConeSolverImpl();
	
	
	
	@Test
	public void chernTestCase() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(1, -1, 3, -8));
		eqSys.add(new IntVector(-1, 2, -1, 1));
		eqSys.add(new IntVector(2, -1, -2, 1));
		eqSys.add(new IntVector(-3, 1, -1, 6));
		eqSys.add(new IntVector(1, 1, -3, 2));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 4, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(15, 11, 12, 5),//
						new IntVector(13, 9, 12, 7),//
						new IntVector(5, 5, 8, 3),//
						new IntVector(1, -1, 2, 1) //
				), solution);
	}
	
	@Test
	public void chernTestCaseMod() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(1, -1, 3, -8, 0));
		eqSys.add(new IntVector(-1, 2, -1, 1, 0));
		eqSys.add(new IntVector(2, -1, -2, 1, 0));
		eqSys.add(new IntVector(-3, 1, -1, 6, 0));
		eqSys.add(new IntVector(1, 1, -3, 2, 0));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 5, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(15, 11, 12, 5, 0),//
						new IntVector(13, 9, 12, 7, 0),//
						new IntVector(5, 5, 8, 3, 0),//
						new IntVector(1, -1, 2, 1, 0) //
				), solution);
	}
	
	@Test
	public void chernTestCase5dOctant() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-1, 0, 0, 0, 0));
		eqSys.add(new IntVector(0, -1, 0, 0, 0));
		eqSys.add(new IntVector(0, 0, -1, 0, 0));
		eqSys.add(new IntVector(0, 0, 0, -1, 0));
		eqSys.add(new IntVector(0, 0, 0, 0, -1));
		eqSys.add(new IntVector(1, -1, 3, -8, 5));
		eqSys.add(new IntVector(-1, 2, -1, 1, -1));
		eqSys.add(new IntVector(2, -1, -2, 1, 0));
		eqSys.add(new IntVector(-3, 1, -1, 6, -3));
		eqSys.add(new IntVector(1, 1, -3, 2, -1));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 5, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(15, 11, 12, 5, 0),//
						new IntVector(13, 0, 17, 8, 0),//
						new IntVector(5, 5, 8, 3, 0),//
						new IntVector(1, 1, 1, 1, 1),//
						new IntVector(13, 9, 12, 7, 0),//
						new IntVector(11, 0, 15, 8, 0),//
						new IntVector(2, 0, 3, 2, 1),//
						new IntVector(5, 0, 9, 4, 0) //
				), solution);
	}
	
	
	
	@Test
	public void simpleBrunoTestCase() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(3, -1, -1));
		eqSys.add(new IntVector(-1, 3, -1));
		eqSys.add(new IntVector(-1, -1, 3));
		eqSys.add(new IntVector(1, -1, 1));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-1, -2, -1),//
						new IntVector(-1, -1, -2),//
						new IntVector(-2, -1, -1) //
				), solution);
	}
	
	@Test
	public void simpleBrunoTestCase2() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(3, -1, -1));
		eqSys.add(new IntVector(-1, 3, -1));
		eqSys.add(new IntVector(3, -1, 2));
		eqSys.add(new IntVector(-1, 3, 2));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-1, -1, -2),//
						new IntVector(-1, -3, 0),//
						new IntVector(-3, -1, 0),//
						new IntVector(-1, -1, 1) //
				), solution);
	}
	
	
	
	@Test
	public void brunoTestCase() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-5, -1, -1, -1));
		eqSys.add(new IntVector(-4, -2, -1, -1));
		eqSys.add(new IntVector(-4, 0, -2, -2));
		eqSys.add(new IntVector(-4, 0, 0, 0));
		eqSys.add(new IntVector(-4, 4, 0, 0));
		eqSys.add(new IntVector(-4, 0, 4, 0));
		eqSys.add(new IntVector(-4, 0, 0, 4));
		eqSys.add(new IntVector(-8, 0, 0, 0));
		eqSys.add(new IntVector(-4, -4, 0, 0));
		eqSys.add(new IntVector(-4, 0, -4, 0));
		eqSys.add(new IntVector(-4, 0, 0, -4));
		
		List <IntVector> solution = coneSolver.solve(eqSys, null, 4, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(1, -1, -1, -1),//
						new IntVector(1, 1, -1, -1),//
						new IntVector(1, 1, 1, 1),//
						new IntVector(1, -1, 1, 1),//
						new IntVector(1, 1, -1, 1),// A
						new IntVector(1, -1, -1, 1),//
						new IntVector(1, 1, 1, -1),//
						new IntVector(1, -1, 1, -1) //
				), solution);
	}
	
	@Test
	public void brunoTestCase2() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(2, 0, 0, 0));
		eqSys.add(new IntVector(2, 1, 0, 0));
		eqSys.add(new IntVector(2, 1, 1, 1));
		eqSys.add(new IntVector(2, 2, 1, 1));
		eqSys.add(new IntVector(2, 1, 2, 1));
		
		List <IntVector> solution = coneSolver.solve(eqSys, null, 4, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(0, 0, -1, 1),//
						new IntVector(0, 0, 1, -2),//
						new IntVector(0, -1, 0, 1),//
						new IntVector(-1, 2, 2, -4),//
						new IntVector(-1, 0, 0, 2) //
				), solution);
	}
	
	
	@Test
	public void simple2dTestCase1() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(1, -4));
		eqSys.add(new IntVector(-2, -1));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 2, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(4, 1),//
						new IntVector(-1, 2) //
				), solution);
	}
	
	@Test
	public void simple2dTestCase2() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-1, 2));
		eqSys.add(new IntVector(2, -1));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 2, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-2, -1),//
						new IntVector(-1, -2) //
				), solution);
	}
	
	@Test
	public void simple2dTestCase3() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-1, 2));
		eqSys.add(new IntVector(3, -3));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 2, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-2, -1),//
						new IntVector(-1, -1) //
				), solution);
	}
	
	
	
	@Test
	public void simple3dTestCase1() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-3, 1, 1));
		eqSys.add(new IntVector(-1, 1, 0));
		eqSys.add(new IntVector(-1, 0, 1));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-1, -1, -2),//
						new IntVector(-1, -2, -1),//
						new IntVector(1, 1, 1) //
				), solution);
	}
	
	@Test
	public void simple3dTestCase2() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-3, 1, 1));
		eqSys.add(new IntVector(-1, 1, 0));
		eqSys.add(new IntVector(-1, 0, 1));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-1, -1, -2),//
						new IntVector(-1, -2, -1),//
						new IntVector(1, 1, 1) //
				), solution);
	}
	
	
	
	@Test
	public void randomNullBasisCase1() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(-9, 8, 0));
		eqSys.add(new IntVector(-9, 0, 7));
		eqSys.add(new IntVector(-6, 2, 1));
		eqSys.add(new IntVector(-3, 4, 0));
		eqSys.add(new IntVector(-3, 0, 5));
		eqSys.add(new IntVector(-2, 2, 2));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-8, -9, -30),//
						new IntVector(-14, -33, -18),//
						new IntVector(0, 0, -1),//
						new IntVector(0, -1, 0),//
						new IntVector(4, 3, 1),//
						new IntVector(5, 2, 3) //
				), solution);
	}
	
	@Test
	public void randomNullBasisCase2() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(9, -8, 0));
		eqSys.add(new IntVector(0, -8, 7));
		eqSys.add(new IntVector(3, -6, 1));
		eqSys.add(new IntVector(3, -4, 0));
		eqSys.add(new IntVector(0, -4, 5));
		eqSys.add(new IntVector(1, -2, 2));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(-34, -21, -24),//
						new IntVector(0, 0, -1),//
						new IntVector(-4, -3, -6),//
						new IntVector(-1, 0, 0),//
						new IntVector(8, 9, 5),//
						new IntVector(2, 5, 4) //
				), solution);
	}
	
	@Test
	public void randomNullBasisCase3() throws Exception {
		List <IntVector> eqSys = new ArrayList <IntVector>();
		eqSys.add(new IntVector(9, 0, -7));
		eqSys.add(new IntVector(0, 8, -7));
		eqSys.add(new IntVector(3, 2, -6));
		eqSys.add(new IntVector(3, 0, -5));
		eqSys.add(new IntVector(0, 4, -5));
		eqSys.add(new IntVector(1, 2, -3));
		List <IntVector> solution = coneSolver.solve(eqSys, null, 3, NullPrintWriter.instance);
		assertSetEquals(//
				Arrays.asList(//
						new IntVector(56, 63, 72),//
						new IntVector(0, -1, 0),//
						new IntVector(-1, 0, 0),//
						new IntVector(-20, -15, -12) //
				), solution);
	}
}
