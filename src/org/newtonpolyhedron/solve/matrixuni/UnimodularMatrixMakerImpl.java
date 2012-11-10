package org.newtonpolyhedron.solve.matrixuni;

import static org.newtonpolyhedron.utils.MatrixUtils.*;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.fs.utils.structure.wrap.Pair;

public class UnimodularMatrixMakerImpl implements UnimodularMatrixMaker {
	
	@Override
	public FieldMatrix <BigFraction> getUnimodularFrom(final FieldMatrix <BigFraction> matrix) {
		if (!matrix.isSquare()) throw new IllegalArgumentException("Non-square matrix");
		
		final FieldMatrix <BigFraction> matrixDiag = matrix.copy();
		final Pair <FieldMatrix <BigFraction>, FieldMatrix <BigFraction>> pair = toDiagonalForm(matrixDiag);
		
		final FieldMatrix <BigFraction> rowOnes = pair.getFirst();
		final FieldMatrix <BigFraction> colOnes = pair.getSecond();
		
		final FieldMatrix <BigFraction> rowOnesInv = inverse(rowOnes);
		final FieldMatrix <BigFraction> colOnesInv = inverse(colOnes);
		
		// Sanity check
		assert matrix.equals(rowOnesInv.multiply(matrixDiag).multiply(colOnesInv));
		
		final FieldMatrix <BigFraction> alpha = rowOnesInv.multiply(colOnesInv);
		return alpha;
	}
}
