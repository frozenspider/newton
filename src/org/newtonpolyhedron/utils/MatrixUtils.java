package org.newtonpolyhedron.utils;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.fraction.BigFractionField;
import org.apache.commons.math3.linear.FieldMatrix;

@Deprecated
public class MatrixUtils {
	
	public static FieldMatrix <BigFraction> create(final BigFraction[][] matrixCopyData) {
		return org.apache.commons.math3.linear.//
		MatrixUtils.createFieldMatrix(matrixCopyData);
	}
	
	public static FieldMatrix <BigFraction> create(final int dimRow, final int dimCol) {
		return org.apache.commons.math3.linear.//
		MatrixUtils.createFieldMatrix(BigFractionField.getInstance(), dimRow, dimCol);
	}
	
	public static FieldMatrix <BigFraction> create(final int dim) {
		return create(dim, dim);
	}
}
