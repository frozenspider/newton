package org.newtonpolyhedron.utils;

import static org.junit.Assert.*;
import static org.newtonpolyhedron.test.TestSupport.*;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.fs.utils.structure.wrap.Pair;
import org.junit.Test;

public class MatrixUtilsTest {
	
	@Test
	public void triangleFormNoSwap() throws Exception {
		int[][] actual = { //
		//
			{1, 2, 3},//
			{2, 5, 8},//
			{3, 7, 12} //
		};
		FieldMatrix <BigFraction> actualM = MatrixUtils.fromInt(actual);
		assertEquals(1, MatrixUtils.toTriangleForm(actualM));
		int[][] expected = { //
		//
			{1, 2, 3},//
			{0, 1, 2},//
			{0, 0, 1} //
		};
		assertMatrixEquals(expected, actualM);
	}
	
	@Test
	public void triangleFormSingleSwap() throws Exception {
		int[][] actual = { //
		//
			{1, 2, 3, 3},//
			{2, 4, 8, 10},//
			{3, 7, 12, 13}, //
			{3, 7, 11, 13} //
		};
		FieldMatrix <BigFraction> actualM = MatrixUtils.fromInt(actual);
		assertEquals(-1, MatrixUtils.toTriangleForm(actualM));
		/*-
		int[][] step1 = { //
		//
			{1, 2, 3, 3},//
			{0, 0, 2, 4},//
			{0, 1, 3, 4},//
			{0, 1, 2, 4},//
		};
		int[][] step2 = { //
		//
			{1, 2, 3, 3},//
			{0, 1, 3, 4},//
			{0, 0, 2, 4},//
			{0, 1, 2, 4},//
		};
		int[][] step3 = { //
		//
			{1, 2, 3, 3},//
			{0, 1, 3, 4},//
			{0, 0, 2, 4},//
			{0, 0, -1, 0},//
		};*/
		int[][] expected = { //
		//
			{1, 2, 3, 3},//
			{0, 1, 3, 4},//
			{0, 0, 2, 4},//
			{0, 0, 0, 2},//
		};
		assertMatrixEquals(expected, actualM);
	}
	
	@Test
	public void inverse1() throws Exception {
		int[][] source = { //
		//
			{+1, 0, 0},//
			{-3, 1, 0},//
			{+0, 0, 1},//
		};
		FieldMatrix <BigFraction> actual = MatrixUtils.inverse(MatrixUtils.fromInt(source));
		int[][] expected = { //
		//
			{1, 0, 0},//
			{3, 1, 0},//
			{0, 0, 1},//
		};
		assertMatrixEquals(expected, actual);
	}
	
	@Test
	public void inverse2() throws Exception {
		int[][] source = { //
		//
			{1, -3, +2},//
			{0, +1, -2},//
			{0, +0, +1},//
		};
		FieldMatrix <BigFraction> actual = MatrixUtils.inverse(MatrixUtils.fromInt(source));
		int[][] expected = { //
		//
			{1, 3, 4},//
			{0, 1, 2},//
			{0, 0, 1},//
		};
		assertMatrixEquals(expected, actual);
	}
	
	@Test
	public void getDet() throws Exception {
		int[][] source = { //
		//
			{4, 6, 6},//
			{4, 5, 5},//
			{4, 7, 9},//
		};
		assertEquals(new BigFraction(-8), MatrixUtils.getDet(MatrixUtils.fromInt(source)));
	}
	
	@Test
	public void getDet2() throws Exception {
		int[][] source = { //
		//
			{1605, -1551, -3586},//
			{2278, -2267, -5287},//
			{2275, -2265, -5283},//
		};
		assertEquals(new BigFraction(1), MatrixUtils.getDet(MatrixUtils.fromInt(source)));
	}
	
	@Test
	public void toDiagonalForm1() throws Exception {
		int[][] source = { //
		//
			{36, 18, 72},//
			{5, 6, 12},//
			{2, 8, 16},//
		};
		int[][] expected = { //
		//
			{1, 0, 0},//
			{0, 2, 0},//
			{0, 0, 504},//
		};
		FieldMatrix <BigFraction> actual = MatrixUtils.fromInt(source);
		Pair <FieldMatrix <BigFraction>, FieldMatrix <BigFraction>> pair = MatrixUtils.toDiagonalForm(actual);
		assertMatrixEquals(expected, actual);
		//
		// row-ones support matrix
		//
		int[][] expectedRowOnes = { //
		//
			{0, -1, 1},//
			{-3, 142, -140},//
			{-14, 666, -657},//
		};
		assertMatrixEquals(expectedRowOnes, pair.getFirst());
		//
		// column-ones support matrix
		//
		int[][] expectedColOnes = { //
		//
			{-1, 2, -108},//
			{-1, 9, -484},//
			{0, -3, 161},//
		};
		assertMatrixEquals(expectedColOnes, pair.getSecond());
	}
	
	@Test
	public void toDiagonalForm2() throws Exception {
		int[][] source = { //
		//
			{0, 1, -1},//
			{2, -3, 0},//
			{0, 0, 0},//
		};
		int[][] expected = { //
		//
			{1, 0, 0},//
			{0, -1, 0},//
			{0, 0, 0},//
		};
		FieldMatrix <BigFraction> actual = MatrixUtils.fromInt(source);
		Pair <FieldMatrix <BigFraction>, FieldMatrix <BigFraction>> pair = MatrixUtils.toDiagonalForm(actual);
		assertMatrixEquals(expected, actual);
		//
		// row-ones support matrix
		//
		int[][] expectedRowOnes = { //
		//
			{1, 0, 0},//
			{3, 1, 0},//
			{0, 0, 1},//
		};
		assertMatrixEquals(expectedRowOnes, pair.getFirst());
		//
		// column-ones support matrix
		//
		int[][] expectedColOnes = { //
		//
			{0, 1, 3},//
			{1, 1, 2},//
			{0, 1, 2},//
		};
		assertMatrixEquals(expectedColOnes, pair.getSecond());
	}
}
