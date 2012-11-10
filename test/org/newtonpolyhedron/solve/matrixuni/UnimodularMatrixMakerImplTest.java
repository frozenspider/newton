package org.newtonpolyhedron.solve.matrixuni;

import static org.newtonpolyhedron.test.TestSupport.*;
import static org.newtonpolyhedron.utils.MatrixUtils.*;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.junit.Test;

public class UnimodularMatrixMakerImplTest {
	
	private static final UnimodularMatrixMakerImpl	maker	= new UnimodularMatrixMakerImpl();
	
	@Test(expected = IllegalArgumentException.class)
	public void illegalArgNonSquare() throws Exception {
		maker.getUnimodularFrom(create(1, 2));
	}
	
	@Test
	public void simple1() throws Exception {
		int[][] source = {//
		//
			{1, 3, 4},//
			{3, 4, 2},//
			{0, 0, 0},//
		};
		int[][] expected = { //
		//
			{1, 3, 4},//
			{3, 10, 14},//
			{0, 0, 1},//
		};
		FieldMatrix <BigFraction> actual = maker.getUnimodularFrom(fromInt(source));
		assertMatrixEquals(expected, actual);
	}
	
	@Test
	public void simple2() throws Exception {
		int[][] source = { //
		//
			{36, 18, 72},//
			{5, 6, 12},//
			{2, 8, 16},//
		};
		int[][] expected = { //
		//
			{1605, -1551, -3586},//
			{2278, -2267, -5287},//
			{2275, -2265, -5283},//
		};
		FieldMatrix <BigFraction> actual = maker.getUnimodularFrom(fromInt(source));
		assertMatrixEquals(expected, actual);
	}
}
