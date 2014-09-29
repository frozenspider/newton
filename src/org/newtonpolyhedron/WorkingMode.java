package org.newtonpolyhedron;

public enum WorkingMode {
	POLY_MOTZKIN_BURGER("Polyhedron (Motzkin - Burger)"), //
	POLY_INTERSECTION("Polyhedron Intersection"), //
	CONE("Cone"), //
	MATRIX_DET("Matrix Determinant"), //
	MATRIX_INVERSE("Matrix Inverse"), //
	MATRIX_UNIMODULAR_ALPHA("Unimodular \"Alpha\"-matrix"), //
	MATRIX_LAST_ROW_MINOR_GCD("Matrix last row minors GCD"), //
	POWER_TRANSFORMATION("Power Transformation"), //
	;
	
	private final String	textValue;
	
	WorkingMode(final String textValue) {
		this.textValue = textValue;
	}
	
	@Override
	public String toString() {
		return textValue;
	}
}