package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrixDetSolverPrinter extends SolverPrinter <Void> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	private final int						skipRow;
	private final int						skipCol;
	
	public MatrixDetSolverPrinter(
			final FieldMatrix <BigFraction> baseMatrix,
			final int skipRow,
			final int skipCol,
			final PrintWriter output) {
		super(null, output);
		this.baseMatrix = baseMatrix;
		this.skipRow = skipRow;
		this.skipCol = skipCol;
	}
	
	@Override
	public void solveFor(final Void nothing, final PrintWriter output) {
		final BigFraction det = MatrixUtils.getDet(baseMatrix, skipRow, skipCol);
		output.println(subheader("Base matrix:"));
		output.println(MatrixUtils.toString(baseMatrix));
		output.println("Matrix determinant: " + det);
	}
}
