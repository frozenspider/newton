package org.newtonpolyhedron.solverprinters;

import static java.text.MessageFormat.*;

import java.io.PrintWriter;
import java.util.List;

import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.cone.ConeSolver;
import org.newtonpolyhedron.utils.MatrixUtils;

public class ConeSolverPrinter extends SolverPrinter <ConeSolver> {
	
	private final List <IntVector>	inequations;
	private final List <IntVector>	basis;
	
	public ConeSolverPrinter(
			final ConeSolver solver,
			final List <IntVector> inequations,
			final List <IntVector> basis,
			final PrintWriter output) {
		super(solver, output);
		this.inequations = inequations;
		this.basis = basis;
	}
	
	@Override
	protected void solveFor(final ConeSolver solver, final PrintWriter output) throws Exception {
		output.println(title("Cone computing"));
		final int rank = MatrixUtils.getRank(MatrixUtils.fromIntVector(inequations));
		output.println("Matrix rank = " + rank);
		output.println(header("Original inequalities:"));
		for (int i = 0; i < inequations.size(); i++) {
			output.println(format(" c{0} = {1}", i + 1, inequations.get(i)));
		}
		final int dim = inequations.get(0).getDim();
		final List <IntVector> solved = solver.solve(inequations, basis, dim, output);
		coneFinalSolutionOutput(solved, output);
	}
	
	private static void coneFinalSolutionOutput(
			final List <IntVector> testing,
			final PrintWriter output) {
		output.println(header("FINAL SOLUTIONS:"));
		for (int i = 0; i < testing.size(); i++) {
			final String str = testing.get(i).toString();
			final int len = str.length();
			output.print(str + "\t");
			if (len < 10) {
				output.print("\t");
			}
			if (len < 16) {
				output.print("\t");
			}
			if ((i + 1) % 4 == 0) {
				output.println();
			}
		}
		output.println(header("ANSWERS:"));
		for (final IntVector p : testing) {
			output.println(p);
		}
	}
}
