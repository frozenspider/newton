package org.newtonpolyhedron.entity.vector;

import java.util.Arrays;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.fraction.BigFractionFormat;
import org.newtonpolyhedron.utils.BigFractionExtFormat;

public class FractionVectorFormat implements VectorFormat <BigFraction, FractionVector> {
	
	private static final FractionVectorFormat	instance	= new FractionVectorFormat();
	private static final BigFractionFormat		frFormat	= new BigFractionExtFormat();
	
	public static FractionVectorFormat getInstance() {
		return instance;
	}
	
	public static BigFractionFormat getFractionFormat() {
		return frFormat;
	}
	
	
	private FractionVectorFormat() {}
	
	@Override
	public BigFraction[] createArrayOfZeros(final int length) {
		final BigFraction[] result = new BigFraction[length];
		Arrays.fill(result, BigFraction.ZERO);
		return result;
	}
	
	@Override
	public BigFraction parseElement(final String src) {
		return frFormat.parse(src);
	}
	
	@Override
	public FractionVector makeVector(final BigFraction[] components) {
		return new FractionVector(components);
	}
	
}
