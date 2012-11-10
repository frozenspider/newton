package org.newtonpolyhedron.entity;

import java.io.PrintWriter;

public abstract class SolverPrinter<T> {
	
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
}
