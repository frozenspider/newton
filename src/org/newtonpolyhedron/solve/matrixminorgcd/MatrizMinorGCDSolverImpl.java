package org.newtonpolyhedron.solve.matrixminorgcd;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.utils.ArithUtils;
import org.newtonpolyhedron.utils.MatrixUtils;

public class MatrizMinorGCDSolverImpl implements MatrizMinorGCDSolver {
	
	@Override
	public Pair <Integer, List <BigInteger>> getLastRowGcd(FieldMatrix <BigFraction> matrix) {
		if (!matrix.isSquare()) {
			throw new IllegalArgumentException("Matrix must be square (although last row is zeros)");
		}
		List <BigInteger> dets = new ArrayList <BigInteger>();
		for (int skipCol = 0; skipCol < matrix.getColumnDimension(); skipCol++) {
			BigFraction det = MatrixUtils.getDet(matrix, matrix.getRowDimension() - 1, skipCol);
			if (!ArithUtils.isInteger(det)) {
				throw new IllegalArgumentException("Non-integer minor");
			}
			dets.add(det.getNumerator());
		}
		
		BigInteger gcd = dets.get(0);
		for (int i = 1; i < dets.size(); ++i) {
			gcd = gcd(gcd, dets.get(i));
		}
		return ImmutablePair.of(gcd.intValue(), dets);
	}
	
	private BigInteger gcd(BigInteger one, BigInteger two) {
		return one.gcd(two);
	}
}
