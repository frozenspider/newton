package org.newtonpolyhedron.entity;

import java.io.PrintWriter;

public class ExecutorRunnable implements Runnable {
	
	private final SolverPrinter <?>	solver;
	private final PrintWriter		output;
	
	public ExecutorRunnable(final SolverPrinter <?> solverPrinter, final PrintWriter output) {
		this.solver = solverPrinter;
		this.output = output;
	}
	
	@Override
	public void run() {
		try {
			solver.solveAndPrint();
		} catch(final Exception ex) {
			ex.printStackTrace(output);
		}
	}
}
