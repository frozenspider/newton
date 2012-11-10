package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.solve.matrixinv.MatrixInverseSolver;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrixInverseSolverPrinter extends SolverPrinter <MatrixInverseSolver> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	
	public MatrixInverseSolverPrinter(
			final MatrixInverseSolver solver,
			final FieldMatrix <BigFraction> baseMatrix,
			final PrintWriter output) {
		super(solver, output);
		this.baseMatrix = baseMatrix;
	}
	
	@Override
	protected void solveFor(final MatrixInverseSolver solver, final PrintWriter output)
			throws Exception {
		final FieldMatrix <BigFraction> inv = solver.getInverse(baseMatrix);
		output.println(subheader("Base matrix:"));
		output.println(MatrixUtils.toString(baseMatrix));
		output.println(subheader("Matrix inverse:"));
		output.println(MatrixUtils.toString(inv));
	}
}
