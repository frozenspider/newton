package org.newtonpolyhedron.solve.matrixinv;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.apache.commons.math3.linear.SingularMatrixException;

public interface MatrixInverseSolver {
	
	/**
	 * Get the inverse of the matrix.
	 * 
	 * @param matrix
	 *            base (square) matrix, [row][column]
	 * @return a matrix inverse
	 * @throws SingularMatrixException
	 *             if matrix is singular
	 */
	public FieldMatrix <BigFraction> getInverse(FieldMatrix <BigFraction> matrix)
			throws SingularMatrixException;
}
