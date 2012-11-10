package org.newtonpolyhedron.test;

import static org.junit.Assert.*;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.utils.MatrixUtils;

public class TestSupport {
	
	public static void assertMatrixEquals(int[][] expected, FieldMatrix <BigFraction> actual) {
		if (expected.length != actual.getRowDimension()) {
			fail("Matrix row dimension differs");
		}
		if (expected[0].length != actual.getColumnDimension()) {
			fail("Matrix column dimension differs");
		}
		assertEquals(MatrixUtils.fromInt(expected), actual);
	}
}
