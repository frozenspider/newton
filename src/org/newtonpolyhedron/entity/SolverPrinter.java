package org.newtonpolyhedron.entity;

import java.io.PrintWriter;

public abstract class SolverPrinter <T> {
	
	private final T				solver;
	private final PrintWriter	output;
	
	public SolverPrinter(final T solver, final PrintWriter output) {
		this.solver = solver;
		this.output = output;
	}
	
	public final void solveAndPrint() throws Exception {
		solveFor(solver, output);
	}
	
	protected abstract void solveFor(T solver, PrintWriter output) throws Exception;
	
	//
	// Print helpers
	//
	public static String title(String title) {
		return "\n\n\n=== === " + title + " === ===\n";
	}
	
	public static String header(String header) {
		return "\n=== " + header + " ===";
	}
	
	public static String subheader(String subheader) {
		return "\n" + subheader + "";
	}
}
