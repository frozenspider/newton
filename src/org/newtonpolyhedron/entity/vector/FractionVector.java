package org.newtonpolyhedron.entity.vector;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.math3.fraction.BigFraction;

public class FractionVector extends AbstractVector <BigFraction, FractionVector> {
	
	/**
	 * Creates a point filled with zeros
	 * 
	 * @param dim
	 */
	public FractionVector(final int dim) {
		super(new BigFraction[dim]);
		Arrays.fill(components, BigFraction.ZERO);
	}
	
	public FractionVector(final long... components) {
		super(new BigFraction[components.length]);
		for (int i = 0; i < components.length; i++) {
			this.components[i] = new BigFraction(components[i]);
		}
	}
	
	public FractionVector(final BigInteger... components) {
		super(new BigFraction[components.length]);
		for (int i = 0; i < components.length; i++) {
			this.components[i] = new BigFraction(components[i]);
		}
	}
	
	public FractionVector(final BigFraction... components) {
		super(Arrays.copyOf(components, components.length));
	}
	
	public FractionVector(final List <BigFraction> components) {
		super(components.toArray(new BigFraction[0]));
	}
	
	public FractionVector(final FractionVector otherVector) {
		super(Arrays.copyOf(otherVector.components, otherVector.components.length));
	}
	
	public FractionVector(final IntVector vector) {
		this(vector.components);
	}
	
	//
	// Arithmetic
	//
	@Override
	protected BigFraction negate(final BigFraction value) {
		return value.negate();
	}
	
	@Override
	protected BigFraction add(final BigFraction one, final BigFraction two) {
		return one.add(two);
	}
	
	@Override
	protected BigFraction subtract(final BigFraction one, final BigFraction two) {
		return one.subtract(two);
	}
	
	@Override
	protected BigFraction multiply(final BigFraction one, final BigFraction two) {
		return one.multiply(two);
	}
	
	@Override
	protected void divide(final BigFraction[] divisor, final BigFraction[] copyResult) {
		for (int i = 0; i < copyResult.length; ++i) {
			copyResult[i] = components[i].divide(divisor[i]);
		}
	}
	
	public FractionVector multiply(final BigInteger coeff) {
		return multiply(new BigFraction(coeff));
	}
	
	public BigFraction dotProduct(final IntVector otherVector) {
		return dotProduct(new FractionVector(otherVector.components));
	}
	
	//
	// Utility
	//
	@Override
	protected FractionVector create(final BigFraction[] data) {
		return new FractionVector(data);
	}
	
	@Override
	protected BigFraction valueOf(final long value) {
		return new BigFraction(value);
	}
	
	public IntVector asInt() {
		return IntVector.fromFractions(components);
	}
	
	
	
	@Override
	protected BigFraction getZeroNumber() {
		return BigFraction.ZERO;
	}
	
	@Override
	protected BigFraction[] createArrayOfLength(final int len) {
		return new BigFraction[len];
	}
	
}
