package org.newtonpolyhedron;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.math3.fraction.BigFraction;
import org.junit.Test;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.FractionVectorFormat;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.entity.vector.VectorFormatOld;

import scala.actors.threadpool.Arrays;

public class NewtonLogicOldTest {
	
	private NewtonLogic	logic	= new NewtonLogic();
	
	@Test
	public void readFromFileSimplest() {
		final List <FractionVector> pointList = new ArrayList <FractionVector>();
		final List <IntVector> commonLimits = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		final FractionVectorFormat format = FractionVectorFormat.getInstance();
		final String content = "3\n" + //
				"1 2 3\n" + //
				"4 5 6\n";
		
		final List <FractionVector> expectedPointList = new ArrayList <FractionVector>();
		expectedPointList.add(new FractionVector(1, 2, 3));
		expectedPointList.add(new FractionVector(4, 5, 6));
		logic.readFromContent(content, pointList, commonLimits, basis, format);
		assertEquals(expectedPointList, pointList);
		assertTrue(commonLimits.isEmpty());
		assertTrue(basis.isEmpty());
	}
	
	@Test
	public void readFromFileSimple() {
		final List <FractionVector> pointList = new ArrayList <FractionVector>();
		final List <IntVector> commonLimits = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		final FractionVectorFormat format = FractionVectorFormat.getInstance();
		final String content = "3\n" + //
				"1.0 2.00 009\n" + //
				"2.22 3/2 6/2\n";
		
		final List <FractionVector> expectedPointList = new ArrayList <FractionVector>();
		expectedPointList.add(new FractionVector(1, 2, 9));
		expectedPointList.add(new FractionVector( //
				BigFraction.getReducedFraction(222, 100), //
				BigFraction.getReducedFraction(3, 2), //
				BigFraction.getReducedFraction(6, 2)));
		logic.readFromContent(content, pointList, commonLimits, basis, format);
		assertEquals(expectedPointList, pointList);
		assertTrue(commonLimits.isEmpty());
		assertTrue(basis.isEmpty());
	}
	
	@Test
	public void readFromFileBasis() {
		final List <FractionVector> pointList = new ArrayList <FractionVector>();
		final List <IntVector> commonLimits = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		final FractionVectorFormat format = FractionVectorFormat.getInstance();
		final String content = "3\n" + //
				"#\n" + //
				"1 0 0\n" + //
				"0 0 1\n" + //
				"3 1 -1\n" + //
				"2 3 4\n" + //
				"#\n" + //
				"1 2 3\n" + //
				"4 5 6\n";
		
		final List <FractionVector> expectedPointList = new ArrayList <FractionVector>();
		expectedPointList.add(new FractionVector(1, 2, 3));
		expectedPointList.add(new FractionVector(4, 5, 6));
		final List <IntVector> expectedBasis = new ArrayList <IntVector>();
		expectedBasis.add(new IntVector(1, 0, 0));
		expectedBasis.add(new IntVector(0, 0, 1));
		expectedBasis.add(new IntVector(3, 1, -1));
		expectedBasis.add(new IntVector(2, 3, 4));
		logic.readFromContent(content, pointList, commonLimits, basis, format);
		assertEquals(expectedPointList, pointList);
		assertTrue(commonLimits.isEmpty());
		assertEquals(expectedBasis, basis);
	}
	
	@Test
	public void readFromFileLimits() {
		final List <FractionVector> pointList = new ArrayList <FractionVector>();
		final List <IntVector> commonLimits = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		final FractionVectorFormat format = FractionVectorFormat.getInstance();
		final String content = "3\n" + //
				"$\n" + //
				"1 0 0\n" + //
				"0 0 1\n" + //
				"3 1 -1\n" + //
				"2 3 4\n" + //
				"$\n" + //
				"1 2 3\n" + //
				"4 5 6\n";
		
		final List <FractionVector> expectedPointList = new ArrayList <FractionVector>();
		expectedPointList.add(new FractionVector(1, 2, 3));
		expectedPointList.add(new FractionVector(4, 5, 6));
		final List <IntVector> expectedLimits = new ArrayList <IntVector>();
		expectedLimits.add(new IntVector(1, 0, 0));
		expectedLimits.add(new IntVector(0, 0, 1));
		expectedLimits.add(new IntVector(3, 1, -1));
		expectedLimits.add(new IntVector(2, 3, 4));
		logic.readFromContent(content, pointList, commonLimits, basis, format);
		assertEquals(expectedPointList, pointList);
		assertEquals(expectedLimits, commonLimits);
		assertTrue(basis.isEmpty());
	}
	
	@Test
	public void readFromFileBoth() {
		final List <FractionVector> pointList = new ArrayList <FractionVector>();
		final List <IntVector> commonLimits = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		final FractionVectorFormat format = FractionVectorFormat.getInstance();
		final String content = " 3\n" + //
				"  $\n" + //
				"1 0 0\n" + //
				"0 0 1\n" + //
				"3 1 -1\n" + //
				"2 3  4\n" + //
				"$\n" + //
				" #\n" + //
				"0 		2 3\n" + //
				" 1 9    2  \n" + //
				"	#\n" + //
				"1 	2.00 3\n" + //
				"  4  10/2 6\n";
		
		final List <FractionVector> expectedPointList = new ArrayList <FractionVector>();
		expectedPointList.add(new FractionVector(1, 2, 3));
		expectedPointList.add(new FractionVector(4, 5, 6));
		final List <IntVector> expectedLimits = new ArrayList <IntVector>();
		expectedLimits.add(new IntVector(1, 0, 0));
		expectedLimits.add(new IntVector(0, 0, 1));
		expectedLimits.add(new IntVector(3, 1, -1));
		expectedLimits.add(new IntVector(2, 3, 4));
		final List <IntVector> expectedBasis = new ArrayList <IntVector>();
		expectedBasis.add(new IntVector(0, 2, 3));
		expectedBasis.add(new IntVector(1, 9, 2));
		logic.readFromContent(content, pointList, commonLimits, basis, format);
		assertEquals(expectedPointList, pointList);
		assertEquals(expectedLimits, commonLimits);
		assertEquals(expectedBasis, basis);
	}
}
