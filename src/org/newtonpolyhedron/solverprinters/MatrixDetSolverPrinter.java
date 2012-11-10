package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.solve.matrixdet.MatrixDetSolver;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrixDetSolverPrinter extends SolverPrinter <MatrixDetSolver> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	private final int						skipRow;
	private final int						skipCol;
	
	public MatrixDetSolverPrinter(
			final MatrixDetSolver solver,
			final FieldMatrix <BigFraction> baseMatrix,
			final int skipRow,
			final int skipCol,
			final PrintWriter output) {
		super(solver, output);
		this.baseMatrix = baseMatrix;
		this.skipRow = skipRow;
		this.skipCol = skipCol;
	}
	
	@Override
	protected void solveFor(final MatrixDetSolver solver, final PrintWriter output)
			throws Exception {
		final BigFraction det = solver.getDet(baseMatrix, skipRow, skipCol);
		output.println();
		output.println("Base matrix: ");
		output.println(MatrixUtils.toString(baseMatrix));
		output.println("Matrix determinant: " + det);
	}
}
