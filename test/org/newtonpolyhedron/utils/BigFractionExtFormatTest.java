package org.newtonpolyhedron.utils;

import static org.junit.Assert.*;

import java.util.Scanner;

import org.apache.commons.math3.exception.MathParseException;
import org.apache.commons.math3.fraction.BigFraction;
import org.junit.Test;

public class BigFractionExtFormatTest {
	
	@Test
	public void parseInteger() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		String str = "123";
		assertEquals(new BigFraction(123), fmt.parse(str));
	}
	
	@Test
	public void parseFraction() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		String str = "123/456";
		assertEquals(new BigFraction(123, 456), fmt.parse(str));
	}
	
	@Test
	public void parseDecimal() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		String str = "123.456";
		assertEquals(new BigFraction(123456, 1000), fmt.parse(str));
	}
	
	@Test
	public void parseDecimalNoLeadingZero() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		String str = ".456";
		assertEquals(new BigFraction(456, 1000), fmt.parse(str));
	}
	
	@Test
	public void parseMultiple() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		String str = "   " //
				+ "123" + " \n " //
				+ "321" + "\t" //
				+ "999/2" + "      " //
				+ "-123.46" + " "//
				+ "asd";
		Scanner scanner = new Scanner(str);
		assertEquals(new BigFraction(123), fmt.parse(scanner.next()));
		assertEquals(new BigFraction(321), fmt.parse(scanner.next()));
		assertEquals(new BigFraction(999, 2), fmt.parse(scanner.next()));
		assertEquals(new BigFraction(-12346, 100), fmt.parse(scanner.next()));
	}
	
	@Test(expected = MathParseException.class)
	public void parseFailString() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		fmt.parse("qwe");
	}
	
	@Test(expected = MathParseException.class)
	public void parseFailWrongSep() throws Exception {
		BigFractionExtFormat fmt = new BigFractionExtFormat();
		fmt.parse(",123");
	}
}
