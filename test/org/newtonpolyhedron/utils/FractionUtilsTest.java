package org.newtonpolyhedron.utils;

import static org.junit.Assert.*;

import java.math.BigInteger;

import org.apache.commons.math3.fraction.BigFraction;
import org.junit.Test;

public class FractionUtilsTest {
	
	@Test
	public void roundTest() throws Exception {
		double[] values = {
			0, 3, 8.0, 8.1, 8.4, 8.49, 8.5, 8.51, 8.6, -4.0, -4.2, -4.49, -0.5, -4.5, -4.8, -4.99};
		for (double val : values) {
			long expected = Math.round(val);
			BigInteger actual = ArithUtils.round(new BigFraction(val));
			assertEquals("For " + val + ": ", expected, actual.longValue());
		}
	}
}
