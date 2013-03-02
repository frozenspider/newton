package org.newtonpolyhedron.utils;

import java.math.BigInteger;

import org.apache.commons.math3.fraction.BigFraction;

public class ArithUtils {
	
	//
	// Comparisons - Fraction with Fraction
	//
	public static boolean greater(final BigFraction what, final BigFraction toWhat) {
		return what.compareTo(toWhat) == 1;
	}
	
	public static boolean less(final BigFraction what, final BigFraction toWhat) {
		return what.compareTo(toWhat) == -1;
	}
	
	public static boolean greaterOrEq(final BigFraction what, final BigFraction toWhat) {
		return !less(what, toWhat);
	}
	
	public static boolean lessOrEq(final BigFraction what, final BigFraction toWhat) {
		return !greater(what, toWhat);
	}
	
	
	//
	// Comparisons - Fraction with long
	//
	public static boolean greater(final BigFraction what, final long toWhat) {
		return greater(what, new BigFraction(toWhat));
	}
	
	public static boolean less(final BigFraction what, final long toWhat) {
		return less(what, new BigFraction(toWhat));
	}
	
	public static boolean greaterOrEq(final BigFraction what, final long toWhat) {
		return !less(what, toWhat);
	}
	
	public static boolean lessOrEq(final BigFraction what, final long toWhat) {
		return !greater(what, toWhat);
	}
	
	
	//
	// Value properties checking
	//
	public static boolean isZero(final BigFraction fraction) {
		return fraction.equals(BigFraction.ZERO);
	}
	
	public static boolean isZero(final BigInteger bigInteger) {
		return bigInteger.equals(BigInteger.ZERO);
	}
	
	public static boolean isInteger(final BigFraction fraction) {
		return fraction.getDenominator().equals(BigInteger.ONE);
	}
	
	
	//
	// Fraction utility
	//
	public static BigInteger getRemainder(final BigFraction div) {
		return div.getNumerator().remainder(div.getDenominator());
	}
	
	/**
	 * @param frac
	 * @return the integer part of fraction (discarding fractional part)
	 */
	public static BigInteger getQuotient(final BigFraction frac) {
		return frac.getNumerator().divide(frac.getDenominator());
	}
}
