package org.newtonpolyhedron.solve.matrixminorgcd;

import static org.junit.Assert.*;
import static org.newtonpolyhedron.test.TestSupport.*;
import static org.newtonpolyhedron.utils.MatrixUtils.*;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.junit.Test;

public class MatrizMinorGCDSolverImplTest {
	
	private static final MatrixMinorGCDSolverImpl	solver	= new MatrixMinorGCDSolverImpl();
	
	@Test
	public void case1() throws Exception {
		int[][] source = {//
		//
			{1, 3, 4},//
			{3, 4, 2},//
			{0, 0, 0},//
		};
		int expectedGcd = 5;
		List <BigInteger> expectedMinors = Arrays.asList( //
				BigInteger.valueOf(-10), //
				BigInteger.valueOf(-10), //
				BigInteger.valueOf(-5) //
		);
		Pair <Integer, List <BigInteger>> actual = solver.getLastRowGcd(fromInt(source));
		assertEquals(new Integer(expectedGcd), actual.getLeft());
		assertEquals(expectedMinors, actual.getRight());
	}
	
	@Test
	public void case2() throws Exception {
		int[][] source = {//
		//
			{-3, -2, 1},//
			{5, -2, 1},//
			{0, 0, 0},//
		};
		int expectedGcd = 8;
		List <BigInteger> expectedMinors = Arrays.asList( //
				BigInteger.valueOf(0), //
				BigInteger.valueOf(-8), //
				BigInteger.valueOf(16) //
		);
		Pair <Integer, List <BigInteger>> actual = solver.getLastRowGcd(fromInt(source));
		assertEquals(new Integer(expectedGcd), actual.getLeft());
		assertEquals(expectedMinors, actual.getRight());
	}
	
	@Test
	public void case3() throws Exception {
		int[][] source = {//
		//
			{1, 3, 5, 1},//
			{1, 2, -6, 9},//
			{2, 4, 3, 2},//
			{0, 0, 0, 0},//
		};
		int expectedGcd = 1;
		List <BigInteger> expectedMinors = Arrays.asList( //
				BigInteger.valueOf(73), //
				BigInteger.valueOf(56), //
				BigInteger.valueOf(16), //
				BigInteger.valueOf(-15) //
		);
		Pair <Integer, List <BigInteger>> actual = solver.getLastRowGcd(fromInt(source));
		assertEquals(new Integer(expectedGcd), actual.getLeft());
		assertEquals(expectedMinors, actual.getRight());
	}
}
