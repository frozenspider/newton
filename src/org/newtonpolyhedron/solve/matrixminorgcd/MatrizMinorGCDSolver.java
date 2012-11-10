package org.newtonpolyhedron.solve.matrixminorgcd;

import java.math.BigInteger;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.apache.commons.math3.linear.SingularMatrixException;

public interface MatrizMinorGCDSolver {
	
	/**
	 * Get the GCD of matrix minors for the last row.
	 * 
	 * @param matrix
	 *            base (non-square) matrix, [row][column], N-1 x N
	 * @return a matrix gcd and minors
	 * @throws SingularMatrixException
	 *             if matrix is singular
	 */
	public Pair <Integer, List <BigInteger>> getLastRowGcd(FieldMatrix <BigFraction> matrix);
}
