package org.newtonpolyhedron.entity;

import static org.junit.Assert.*;

import java.math.BigInteger;

import org.junit.Test;
import org.newtonpolyhedron.entity.vector.IntVector;

public class IntVectorTest {
	
	@Test
	public void reducing1() throws Exception {
		IntVector expected = new IntVector(-1, 3, 5);
		IntVector actual = IntVector.makeReduced(-2, 6, 10);
		assertEquals(expected, actual);
		assertEquals(-1, actual.get(0).intValue());
		assertEquals(+3, actual.get(1).intValue());
		assertEquals(+5, actual.get(2).intValue());
	}
	
	@Test
	public void reducing2() throws Exception {
		IntVector v1 = new IntVector(-1, 3, 5);
		IntVector v2 = new IntVector(3, 1, 3);
		IntVector expectedSum = new IntVector(1, 2, 4);
		IntVector actualSum = v1.add(v2).getReduced();
		assertEquals(expectedSum, actualSum);
	}
	
	
	@Test
	public void add() throws Exception {
		IntVector v1 = new IntVector(100, 200, 300);
		IntVector v2 = new IntVector(300, 200, 100);
		IntVector expectedSum = new IntVector(400, 400, 400);
		IntVector actualSum = v1.add(v2);
		assertEquals(expectedSum, actualSum);
	}
	
	@Test
	public void subtract() throws Exception {
		IntVector v1 = new IntVector(100, 200, 300);
		IntVector v2 = new IntVector(300, 200, 100);
		IntVector expectedDif = new IntVector(-200, 0, 200);
		IntVector actualDif = v1.subtract(v2);
		assertEquals(expectedDif, actualDif);
	}
	
	@Test
	public void multiply() throws Exception {
		IntVector v1 = new IntVector(2, 5, 6);
		IntVector v2 = new IntVector(5, 3, 5);
		IntVector expectedProd = new IntVector(10, 15, 30);
		IntVector actualProd = v1.multiply(v2);
		assertEquals(expectedProd, actualProd);
	}
	
	@Test
	public void divide1() throws Exception {
		IntVector v1 = new IntVector(2, 5, 6);
		IntVector v2 = new IntVector(2, 5, 6);
		IntVector expectedQuot = new IntVector(1, 1, 1);
		IntVector actualQuot = v1.divide(v2);
		assertEquals(expectedQuot, actualQuot);
	}
	
	@Test
	public void divide2() throws Exception {
		IntVector v1 = new IntVector(1, 3, 5);
		IntVector v2 = new IntVector(2, 5, 6);
		IntVector expectedQuot = new IntVector(15, 18, 25);
		IntVector actualQuot = v1.divide(v2);
		assertEquals(expectedQuot, actualQuot);
	}
	
	@Test
	public void divide3() throws Exception {
		IntVector v1 = new IntVector(8, 7, 25);
		IntVector v2 = new IntVector(3, 6, 5);
		IntVector expectedQuot = new IntVector(16, 7, 30);
		IntVector actualQuot = v1.divide(v2);
		assertEquals(expectedQuot, actualQuot);
	}
	
	@Test
	public void dotProduct() throws Exception {
		IntVector v1 = new IntVector(8, 3, 4);
		IntVector v2 = new IntVector(2, 2, 3);
		BigInteger expectedDotProd = BigInteger.valueOf(16 + 6 + 12);
		assertEquals(expectedDotProd, v1.dotProduct(v2));
	}
	
	@Test
	public void withValueAt() throws Exception {
		IntVector v1 = new IntVector(8, 7, 25);
		IntVector expected = new IntVector(8, 7, 250);
		assertEquals(expected, v1.withValueAt(2, 250));
	}
}
