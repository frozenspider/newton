package org.newtonpolyhedron.solve.matrixuni;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;

public interface UnimodularMatrixMaker {
	
	/**
	 * Makes an uniumodular matrix "alpha", that can be used in further Power Transformation
	 * process.
	 * <p>
	 * For more info, see Bruno's "Power Geometry".
	 * 
	 * @param matrix
	 *            source matrix with arbitary determinant
	 * @return unimodular "alpha"-matrix (i.e. {@code det(alpha) == 1})
	 */
	public FieldMatrix <BigFraction> getUnimodularFrom(FieldMatrix <BigFraction> matrix);
}