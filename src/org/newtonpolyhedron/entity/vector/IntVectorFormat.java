package org.newtonpolyhedron.entity.vector;

import java.math.BigInteger;
import java.util.Arrays;

public class IntVectorFormat implements VectorFormat <BigInteger, IntVector> {
	
	private static final IntVectorFormat	instance	= new IntVectorFormat();
	
	public static IntVectorFormat getInstance() {
		return instance;
	}
	
	
	@Override
	public BigInteger[] createArrayOfZeros(final int length) {
		final BigInteger[] result = new BigInteger[length];
		Arrays.fill(result, BigInteger.ZERO);
		return result;
	}
	
	private IntVectorFormat() {}
	
	@Override
	public BigInteger parseElement(final String src) {
		return new BigInteger(src);
	}
	
	@Override
	public IntVector makeVector(final BigInteger[] components) {
		return new IntVector(components);
	}
	
}
