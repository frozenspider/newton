package org.newtonpolyhedron.ex;

import org.newtonpolyhedron.WorkingMode;

public class UnknownModeException extends Exception {
	
	private static final long	serialVersionUID	= 7864040664662041730L;
	
	public UnknownModeException(final WorkingMode mode) {
		super(String.valueOf(mode));
	}
}
