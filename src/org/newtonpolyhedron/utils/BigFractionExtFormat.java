package org.newtonpolyhedron.utils;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParsePosition;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.fraction.BigFractionFormat;

/**
 * BigFraction format, which can parse the following formats of fractions:
 * 
 * <pre>
 * 123
 * 123/456
 * 123.456
 * .456
 * </pre>
 * 
 * @author FS
 */
public class BigFractionExtFormat extends BigFractionFormat {
	
	private static final long	serialVersionUID	= -4307613418262543447L;
	
	@Override
	public BigFraction parse(final String source, final ParsePosition pos) {
		// Try to parse fraction as usual
		final BigFraction superResult = super.parse(source, pos);
		if (superResult != null) return superResult;
		
		// Usual parsing failed (errorIndex has been set)
		final int initialIndex = pos.getIndex();
		
		// Parse whitespace
		parseAndIgnoreWhitespace(source, pos);
		
		// Parse decimal
		final BigDecimal dec = parseNextBigDecimal(source, pos);
		if (dec == null) {
			pos.setIndex(initialIndex);
			return null;
		}
		
		// Unset error index
		pos.setErrorIndex(-1);
		
		final BigInteger unscaled = dec.unscaledValue();
		
		final int scale = dec.scale();
		
		final BigFraction result;
		if (scale >= 0) {
			final BigInteger denom = BigInteger.TEN.pow(scale);
			result = new BigFraction(unscaled, denom);
		} else {
			result = new BigFraction(unscaled.multiply(BigInteger.TEN.pow(-scale)));
		}
		
		return result;
	}
	
	private BigDecimal parseNextBigDecimal(final String source, final ParsePosition pos) {
		final int start = pos.getIndex();
		int end = source.charAt(start) == '-' ? start + 1 : start;
		while (end < source.length()
				&& (Character.isDigit(source.charAt(end)) || source.charAt(end) == '.')) {
			++end;
		}
		
		try {
			final BigDecimal n = new BigDecimal(source.substring(start, end));
			pos.setIndex(end);
			return n;
		} catch(final NumberFormatException nfe) {
			// Do not change the error index, it's already set by super.parse() call
			return null;
		}
	}
}
