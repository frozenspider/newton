package org.newtonpolyhedron.solve.matrixdet;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrixDetSolverImpl implements MatrixDetSolver {
	
	@Override
	public BigFraction getDet(
			final FieldMatrix <BigFraction> matrix,
			final int skipRow,
			final int skipCol) throws ArithmeticException {
		return MatrixUtils.getDet(matrix, skipRow, skipCol);
	}
}
