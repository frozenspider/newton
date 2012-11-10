package org.newtonpolyhedron.entity;

import java.io.PrintWriter;

import org.newtonpolyhedron.utils.StringUtils;

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
	@SuppressWarnings("static-access")
	public static String title(String title) {
		String bordered = "======= " + title + " =======";
		String line = StringUtils.repeat("=", bordered.length());
		return "\n\n\n" + line + "\n" + bordered + "\n" + line;
	}
	
	public static String header(String header) {
		return "\n=== " + header + " ===";
	}
	
	public static String subheader(String subheader) {
		return "\n" + subheader + "";
	}
}
