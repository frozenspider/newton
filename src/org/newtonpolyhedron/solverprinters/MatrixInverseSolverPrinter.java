package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrixInverseSolverPrinter extends SolverPrinter <Void> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	
	public MatrixInverseSolverPrinter(final FieldMatrix <BigFraction> baseMatrix, final PrintWriter output) {
		super(null, output);
		this.baseMatrix = baseMatrix;
	}
	
	@Override
	protected void solveFor(final Void nothing, final PrintWriter output) throws Exception {
		final FieldMatrix <BigFraction> inv = MatrixUtils.inverse(baseMatrix);
		output.println(subheader("Base matrix:"));
		output.println(MatrixUtils.toString(baseMatrix));
		output.println(subheader("Matrix inverse:"));
		output.println(MatrixUtils.toString(inv));
	}
}
