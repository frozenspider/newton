package org.newtonpolyhedron.solve.matrixinv;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.apache.commons.math3.linear.SingularMatrixException;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrixInverseSolverImpl implements MatrixInverseSolver {
	
	@Override
	public FieldMatrix <BigFraction> getInverse(final FieldMatrix <BigFraction> matrix)
			throws SingularMatrixException {
		return MatrixUtils.inverse(matrix);
	}
}