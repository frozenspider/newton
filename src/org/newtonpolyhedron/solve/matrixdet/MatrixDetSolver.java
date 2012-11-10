package org.newtonpolyhedron.solve.matrixdet;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;

public interface MatrixDetSolver {
	
	/**
	 * Computes a matrix determinant or minor - at least part of minor, it will simply skip row and
	 * column with given index (if it's non-negative)
	 * 
	 * @param matrix
	 *            base (square) matrix, [row][column]
	 * @param skipRow
	 *            index of row to skip (minor computation), {@code -1} to ignore
	 * @param skipCol
	 *            index of column to skip (minor computation), {@code -1} to ignore
	 * @return a matrix/submatrix determinant
	 * @throws ArithmeticException
	 *             if input values was somewhat wrong - e.g. skip index > dimension
	 */
	public BigFraction getDet(FieldMatrix <BigFraction> matrix, int skipRow, int skipCol)
			throws ArithmeticException;
}
