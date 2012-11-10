package org.newtonpolyhedron.utils;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;

public class NullPrintWriter extends PrintWriter {
	
	public static final NullPrintWriter	instance;
	static {
		final OutputStream nullOutputStream = new OutputStream() {
			
			@Override
			public void write(final int b) throws IOException {}
		};
		instance = new NullPrintWriter(nullOutputStream);
	}
	
	private NullPrintWriter(final OutputStream nullOutputStream) {
		super(nullOutputStream);
	}
}
