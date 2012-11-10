package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker;
import org.newtonpolyhedron.utils.MatrixUtils;
import org.newtonpolyhedron.utils.StringUtils;

public class UnimodularMatrixMakerPrinter extends SolverPrinter <UnimodularMatrixMaker> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	
	public UnimodularMatrixMakerPrinter(
			final UnimodularMatrixMaker solver,
			final FieldMatrix <BigFraction> baseMatrix,
			final PrintWriter output) {
		super(solver, output);
		this.baseMatrix = baseMatrix;
	}
	
	@Override
	protected void solveFor(final UnimodularMatrixMaker solver, final PrintWriter output)
			throws Exception {
		final FieldMatrix <BigFraction> alpha = solver.getUnimodularFrom(baseMatrix);
		output.println(title("Unimodular \"Alpha\" matrix"));
		StringBuilder text1 = new StringBuilder();
		text1.append(subheader("Base matrix:") + "\n");
		text1.append(MatrixUtils.toString(baseMatrix) + "\n");
		StringBuilder text2 = new StringBuilder();
		text2.append(subheader("Alpha-matrix:") + "\n");
		text2.append(MatrixUtils.toString(alpha) + "\n");
		StringBuilder text3 = new StringBuilder();
		text3.append(subheader("Inverse alpha-matrix:") + "\n");
		text3.append(MatrixUtils.toString(MatrixUtils.inverse(alpha)) + "\n");
		output.println(StringUtils.appendToRight(5, text1, text2, text3));
	}
}
