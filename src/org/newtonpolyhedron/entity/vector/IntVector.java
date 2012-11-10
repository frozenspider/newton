package org.newtonpolyhedron.entity.vector;

import static java.math.BigInteger.*;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.math3.fraction.BigFraction;

/**
 * Immutable integer vector representation.
 * 
 * @author FS
 */
public class IntVector extends AbstractVector <BigInteger, IntVector> {
	
	public static IntVector empty(final int dim) {
		return new IntVector(dim);
	}
	
	public static IntVector fromFractions(final BigFraction... components) {
		return new IntVector(components);
	}
	
	public static IntVector makeReduced(final long... components) {
		return new IntVector(components).getReduced();
	}
	
	public static IntVector makeReduced(final BigInteger... components) {
		return new IntVector(components).getReduced();
	}
	
	public static IntVector fromFractions(final FractionVector fractionVector) {
		return fromFractions(fractionVector.components);
	}
	
	/**
	 * Creates a zero vector.
	 * 
	 * @param dim
	 */
	private IntVector(final int dim) {
		super(new BigInteger[dim]);
		Arrays.fill(components, ZERO);
	}
	
	/**
	 * Creates a reduced vector (with components GCD = 1) from fractions.
	 * 
	 * @param components
	 *            fractions to be used as components base
	 */
	private IntVector(final BigFraction... components) {
		super(new BigInteger[components.length]);
		BigInteger multiplier = ONE;
		for (final BigFraction frac : components) {
			final BigInteger denom = frac.getDenominator();
			if (!multiplier.remainder(denom).equals(ZERO)) {
				multiplier = multiplier.multiply(denom);
			}
		}
		for (int i = 0; i < components.length; i++) {
			final BigInteger num = components[i].getNumerator();
			final BigInteger denom = components[i].getDenominator();
			this.components[i] = num.multiply(multiplier.divide(denom));
		}
		
		// Reducing
		reduce(this.components, getGcd(this.components));
	}
	
	public IntVector(final long... components) {
		super(new BigInteger[components.length]);
		for (int i = 0; i < components.length; i++) {
			this.components[i] = valueOf(components[i]);
		}
	}
	
	public IntVector(final BigInteger... components) {
		super(Arrays.copyOf(components, components.length));
	}
	
	public IntVector(final List <BigInteger> components) {
		this(components.toArray(new BigInteger[0]));
	}
	
	//
	// Arithmetic
	//
	@Override
	protected BigInteger negate(final BigInteger value) {
		return value.negate();
	}
	
	@Override
	protected BigInteger add(final BigInteger one, final BigInteger two) {
		return one.add(two);
	}
	
	@Override
	protected BigInteger subtract(final BigInteger one, final BigInteger two) {
		return one.subtract(two);
	}
	
	@Override
	protected BigInteger multiply(final BigInteger one, final BigInteger two) {
		return one.multiply(two);
	}
	
	@Override
	protected void divide(final BigInteger[] divisor, final BigInteger[] result) {
		final BigFraction[] target = new BigFraction[result.length];
		for (int i = 0; i < result.length; i++) {
			target[i] = new BigFraction(components[i], divisor[i]);
		}
		System.arraycopy(new IntVector(target).components, 0, result, 0, result.length);
	}
	
	public IntVector getReduced() {
		final BigInteger gcd = getGcd(components);
		if (gcd.equals(ZERO) || gcd.equals(ONE)) return this;
		final BigInteger[] target = Arrays.copyOf(components, components.length);
		reduce(target, gcd);
		return new IntVector(target);
	}
	
	/**
	 * Multiplies integer vector by numerator and denominator of a fraction.
	 * 
	 * @param coeff
	 * @return resulting vector
	 */
	public IntVector multByFrac(final BigFraction coeff) {
		return multiply(coeff.getNumerator()).multiply(coeff.getDenominator());
	}
	
	//
	// Internal
	//
	/**
	 * Computes GCD - positive greatest common divisor of all elements.
	 * 
	 * @param bigInts
	 * @return
	 */
	private BigInteger getGcd(final BigInteger[] bigInts) {
		BigInteger gcd = null;
		for (final BigInteger comp : bigInts) {
			if (gcd == null) {
				gcd = comp.abs();
			} else {
				gcd = gcd.gcd(comp);
			}
		}
		return gcd == null ? BigInteger.ZERO : gcd;
	}
	
	/**
	 * Reduce a provided int array by a provided greatest common divisor (obtained via
	 * {@link #getGcd(BigInteger[])})
	 * 
	 * @param target
	 *            array to be reduced
	 * @param gcd
	 *            greatest common divisor
	 */
	private void reduce(final BigInteger[] target, final BigInteger gcd) {
		if (gcd.equals(ZERO) || gcd.equals(ONE)) return;
		for (int i = 0; i < target.length; ++i) {
			target[i] = target[i].divide(gcd);
		}
	}
	
	//
	// Utility
	//
	@Override
	protected IntVector create(final BigInteger[] data) {
		return new IntVector(data);
	}
	
	@Override
	protected BigInteger valueOf(final long value) {
		return BigInteger.valueOf(value);
	}
	
	@Override
	protected BigInteger getZeroNumber() {
		return ZERO;
	}
	
	@Override
	protected BigInteger[] createArrayOfLength(final int len) {
		return new BigInteger[len];
	}
	
	
}
