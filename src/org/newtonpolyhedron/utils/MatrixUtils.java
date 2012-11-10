package org.newtonpolyhedron.utils;

import static org.newtonpolyhedron.utils.ArithUtils.*;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.fraction.BigFractionField;
import org.apache.commons.math3.fraction.FractionConversionException;
import org.apache.commons.math3.linear.FieldLUDecomposition;
import org.apache.commons.math3.linear.FieldMatrix;
import org.apache.commons.math3.linear.FieldVector;
import org.apache.commons.math3.linear.SingularMatrixException;
import org.fs.utils.structure.wrap.Pair;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;

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
	
	public static FieldMatrix <BigFraction> fromInt(final int[][] matrix)
			throws FractionConversionException {
		if (matrix.length == 0) throw new IllegalArgumentException("Zero-sized matrix");
		final BigFraction[][] content = new BigFraction[matrix.length][matrix[0].length];
		for (int i = 0; i < matrix.length; i++) {
			for (int j = 0; j < matrix[0].length; j++) {
				content[i][j] = new BigFraction(matrix[i][j]);
			}
		}
		return create(content);
	}
	
	public static FieldMatrix <BigFraction> fromIntVector(final List <IntVector> vectors)
			throws FractionConversionException {
		final BigFraction[][] content = new BigFraction[vectors.size()][vectors.size() == 0 ? 0
				: vectors.get(0).getDim()];
		int i = 0;
		for (final IntVector vector : vectors) {
			for (int j = 0; j < vector.getDim(); j++) {
				content[i][j] = new BigFraction(vector.get(j));
			}
			++i;
		}
		return create(content);
	}
	
	public static FieldMatrix <BigFraction> fromFracVector(final List <FractionVector> vectors)
			throws FractionConversionException {
		final BigFraction[][] content = new BigFraction[vectors.size()][vectors.size() == 0 ? 0
				: vectors.get(0).getDim()];
		int i = 0;
		for (final FractionVector vector : vectors) {
			for (int j = 0; j < vector.getDim(); j++) {
				content[i][j] = vector.get(j);
			}
			++i;
		}
		return create(content);
	}
	
	public static FieldMatrix <BigFraction> copy(final FieldMatrix <BigFraction> source) {
		return source.copy();
	}
	
	/**
	 * @param dim
	 * @return diagonal matrix of ones
	 */
	public static FieldMatrix <BigFraction> identity(final int dim) {
		final BigFraction[] diagonal = new BigFraction[dim];
		Arrays.fill(diagonal, BigFraction.ONE);
		return org.apache.commons.math3.linear.//
		MatrixUtils.createFieldDiagonalMatrix(diagonal);
	}
	
	public static FieldMatrix <BigFraction> inverse(final FieldMatrix <BigFraction> matrix)
			throws SingularMatrixException {
		if (!matrix.isSquare()) throw new IllegalArgumentException("Non-square matrix");
		
		final FieldMatrix <BigFraction> inverse = new FieldLUDecomposition <BigFraction>(matrix).getSolver().getInverse();
		return inverse;
	}
	
	public static int getRank(final FieldMatrix <BigFraction> matrix) {
		if (matrix.getRowDimension() == 0) throw new IllegalArgumentException("0-row matrix");
		if (matrix.getColumnDimension() == 0)
			throw new IllegalArgumentException("0-column matrix");
		
		{
			// Zero matrix check
			boolean zeroMatrix = true;
			outer: for (int rowIdx = 0; rowIdx < matrix.getRowDimension(); ++rowIdx) {
				for (int colIdx = 0; colIdx < matrix.getColumnDimension(); ++colIdx) {
					if (!isZero(matrix.getEntry(rowIdx, colIdx))) {
						zeroMatrix = false;
						break outer;
					}
				}
			}
			if (zeroMatrix) return 0;
		}
		
		final FieldMatrix <BigFraction> matrixCopy = matrix.copy();
		toTriangleForm(matrixCopy);
		int rank = 0;
		
		final int dim = Math.min(matrix.getRowDimension(), matrix.getColumnDimension());
		while (rank < dim && !isZero(matrixCopy.getEntry(rank, rank))) {
			++rank;
		}
		return rank;
	}
	
	public static BigFraction getDet(
			final FieldMatrix <BigFraction> matrix,
			final int skipRow,
			final int skipCol) throws ArithmeticException {
		final int rowDim = matrix.getRowDimension();
		final int colDim = matrix.getColumnDimension();
		if (skipCol >= colDim) throw new ArithmeticException("skipCol >= dim");
		final BigFraction[][] matrixCopyData = new BigFraction[skipRow == -1 ? rowDim : rowDim - 1][];
		int rowIdx = 0;
		if (skipCol == -1) {
			for (int i = 0; i < rowDim; i++) {
				if (i == skipRow) {
					continue;
				}
				matrixCopyData[rowIdx++] = Arrays.copyOf(matrix.getRow(i), colDim);
			}
		} else {
			for (int i = 0; i < rowDim; i++) {
				if (i == skipRow) {
					continue;
				}
				final BigFraction[] row = new BigFraction[colDim - 1];
				int colIdx = 0;
				for (int j = 0; j < colDim; j++) {
					if (skipCol != j) {
						row[colIdx++] = matrix.getEntry(i, j);
					}
				}
				matrixCopyData[rowIdx++] = row;
			}
		}
		return getDet(create(matrixCopyData));
	}
	
	public static BigFraction getDet(final FieldMatrix <BigFraction> matrix) {
		if (!matrix.isSquare()) throw new IllegalArgumentException("Non-square matrix");
		final BigFraction det = new FieldLUDecomposition <BigFraction>(matrix).getDeterminant();
		
		return det;
	}
	
	/**
	 * Converges the matrix to a triangle form, where all elements below main diagonal are zeros.
	 * <p>
	 * This operation doesn't change determinant value, but it may change it's sign.
	 * <p>
	 * Example of a triangle matrix (empty cells = zeros):
	 * 
	 * <pre>
	 * +--+--+--+--+
	 * | 1| 2| 3| 4|
	 * +--+--+--+--+
	 * |  | 2| 3| 4|
	 * +--+--+--+--+
	 * |  |  | 3| 4|
	 * +--+--+--+--+
	 * |  |  |  | 4|
	 * +--+--+--+--+
	 * </pre>
	 * 
	 * @param matrix
	 *            a matrix to be triangularized
	 * @return {@code 1} or {@code -1} depending on whether or not determinant sign was reversed
	 */
	public static int toTriangleForm(final FieldMatrix <BigFraction> matrix) {
		
		final int rowDim = matrix.getRowDimension();
		final int colDim = matrix.getColumnDimension();
		final int minDim = Math.min(rowDim, colDim);
		
		boolean signPlus = true;
		
		// currIdx tracks a corner element - row and column index
		for (int currIdx = 0; currIdx < minDim; currIdx++) {
			
			BigFraction cornerElement = matrix.getEntry(currIdx, currIdx);
			int swapRowIdx = currIdx;
			boolean currColumnContainsOnlyZeros = false;
			
			// Scroll through column elements below current
			while (isZero(cornerElement)) {
				++swapRowIdx;
				if (swapRowIdx == rowDim) {
					// Nowhere to scroll = no non-zero elements
					currColumnContainsOnlyZeros = true;
					break;
				}
				cornerElement = matrix.getEntry(swapRowIdx, currIdx);
			}
			
			if (currColumnContainsOnlyZeros) {
				// Nothing to do here - just continue to next row
				// Note, that this means, that determinant is zero
				continue;
			}
			
			// Swap rows
			if (swapRowIdx != currIdx) {
				signPlus = !signPlus; // Swap the sign
				
				final FieldVector <BigFraction> row = matrix.getRowVector(currIdx);
				matrix.setRowVector(currIdx, matrix.getRowVector(swapRowIdx));
				matrix.setRowVector(swapRowIdx, row);
			}
			
			final FieldVector <BigFraction> currRow = matrix.getRowVector(currIdx);
			for (int i = currIdx + 1; i < rowDim; i++) {
				FieldVector <BigFraction> otherRow = matrix.getRowVector(i);
				final BigFraction coeff = otherRow.getEntry(currIdx).divide(
						currRow.getEntry(currIdx));
				
				otherRow = otherRow.subtract(currRow.mapMultiply(coeff));
				
				matrix.setRowVector(i, otherRow);
			}
		}
		return signPlus ? 1 : -1;
	}
	
	public static Pair <FieldMatrix <BigFraction>, FieldMatrix <BigFraction>> toDiagonalForm(
			final FieldMatrix <BigFraction> matrix) {
		if (!matrix.isSquare()) throw new IllegalArgumentException("Non-square matrix");
		
		final FieldMatrix <BigFraction> rowOnes = identity(matrix.getRowDimension());
		final FieldMatrix <BigFraction> colOnes = rowOnes.copy();
		toDiagonalForm(matrix, 0, rowOnes, colOnes);
		return new Pair <FieldMatrix <BigFraction>, FieldMatrix <BigFraction>>(rowOnes, colOnes);
	}
	
	//
	// Diagonal form and it's supportive
	//
	private static void toDiagonalForm(
			final FieldMatrix <BigFraction> matrix,
			final int cornerIdx,
			final FieldMatrix <BigFraction> rowOnes,
			final FieldMatrix <BigFraction> colOnes) {
		final int dim = matrix.getRowDimension();
		BigFraction corner = matrix.getEntry(cornerIdx, cornerIdx);
		boolean atLeastOneFails;
		
		// Make corner element a GCD of it's row and column
		do {
			atLeastOneFails = false;
			for (int colIdx = cornerIdx + 1; colIdx < dim;) {
				final BigFraction current = matrix.getEntry(cornerIdx, colIdx);
				if (isZero(corner)) {
					if (!isZero(current)) {
						atLeastOneFails = true;
						swapCols(matrix, cornerIdx, colIdx);
						inverseCol(matrix, colIdx);
						
						swapCols(colOnes, cornerIdx, colIdx);
						inverseCol(colOnes, colIdx);
						
						corner = matrix.getEntry(cornerIdx, cornerIdx);
					}
					++colIdx;
				} else {
					final BigFraction div = current.divide(corner);
					final BigInteger remainder = ArithUtils.getRemainder(div);
					if (!remainder.equals(BigInteger.ZERO)) {
						atLeastOneFails = true;
						final BigInteger quotient = ArithUtils.getQuotient(div);
						
						subtractMultipliedCol(matrix, cornerIdx, colIdx, quotient);
						swapCols(matrix, cornerIdx, colIdx);
						inverseCol(matrix, colIdx);
						
						subtractMultipliedCol(colOnes, cornerIdx, colIdx, quotient);
						swapCols(colOnes, cornerIdx, colIdx);
						inverseCol(colOnes, colIdx);
						
						corner = matrix.getEntry(cornerIdx, cornerIdx);
					} else {
						++colIdx;
					}
				}
			}
			for (int rowIdx = cornerIdx + 1; rowIdx < dim;) {
				final BigFraction current = matrix.getEntry(rowIdx, cornerIdx);
				if (isZero(corner)) {
					if (!isZero(current)) {
						atLeastOneFails = true;
						swapRows(matrix, cornerIdx, rowIdx);
						inverseRow(matrix, rowIdx);
						
						swapRows(rowOnes, cornerIdx, rowIdx);
						inverseRow(rowOnes, rowIdx);;
						
						corner = matrix.getEntry(cornerIdx, cornerIdx);
					}
					++rowIdx;
				} else {
					final BigFraction div = current.divide(corner);
					final BigInteger remainder = ArithUtils.getRemainder(div);
					if (!remainder.equals(BigInteger.ZERO)) {
						atLeastOneFails = true;
						final BigInteger quotient = ArithUtils.getQuotient(div);
						
						subtractMultipliedRow(matrix, cornerIdx, rowIdx, quotient);
						swapRows(matrix, cornerIdx, rowIdx);
						inverseRow(matrix, rowIdx);
						
						subtractMultipliedRow(rowOnes, cornerIdx, rowIdx, quotient);
						swapRows(rowOnes, cornerIdx, rowIdx);
						inverseRow(rowOnes, rowIdx);
						
						corner = matrix.getEntry(cornerIdx, cornerIdx);
					} else {
						++rowIdx;
					}
				}
			}
		} while (atLeastOneFails);
		
		// Subtract multiplied first row/column from others rows/columns
		// so that corner element remains only non-zero element in it's row/column
		for (int colIdx = cornerIdx + 1; colIdx < dim; ++colIdx) {
			final BigFraction current = matrix.getEntry(cornerIdx, colIdx);
			final BigInteger quotient = ArithUtils.getQuotient(current.divide(corner));
			if (!isZero(quotient)) {
				subtractMultipliedCol(matrix, cornerIdx, colIdx, quotient);
				subtractMultipliedCol(colOnes, cornerIdx, colIdx, quotient);
			}
		}
		for (int rowIdx = cornerIdx + 1; rowIdx < dim; ++rowIdx) {
			final BigFraction current = matrix.getEntry(rowIdx, cornerIdx);
			final BigInteger quotient = ArithUtils.getQuotient(current.divide(corner));
			if (!isZero(quotient)) {
				subtractMultipliedRow(matrix, cornerIdx, rowIdx, quotient);
				subtractMultipliedRow(rowOnes, cornerIdx, rowIdx, quotient);
			}
		}
		
		if (cornerIdx < dim - 1) {
			toDiagonalForm(matrix, cornerIdx + 1, rowOnes, colOnes);
		}
	}
	
	/**
	 * {@code dst = dst - src*quot}
	 * 
	 * @param matrix
	 * @param srcColIdx
	 * @param dstColIdx
	 * @param quotient
	 */
	private static void subtractMultipliedCol(
			final FieldMatrix <BigFraction> matrix,
			final int srcColIdx,
			final int dstColIdx,
			final BigInteger quotient) {
		final FieldVector <BigFraction> srcCol = matrix.getColumnVector(srcColIdx);
		final FieldVector <BigFraction> dstCol = matrix.getColumnVector(dstColIdx);
		final FieldVector <BigFraction> srcColMul = srcCol.mapMultiply(new BigFraction(quotient));
		final FieldVector <BigFraction> dstColSub = dstCol.subtract(srcColMul);
		matrix.setColumnVector(dstColIdx, dstColSub);
	}
	
	/**
	 * {@code dst = dst - src*quot}
	 * 
	 * @param matrix
	 * @param srcColIdx
	 * @param dstColIdx
	 * @param quotient
	 */
	private static void subtractMultipliedRow(
			final FieldMatrix <BigFraction> matrix,
			final int srcRowIdx,
			final int dstRowIdx,
			final BigInteger quotient) {
		final FieldVector <BigFraction> srcRow = matrix.getRowVector(srcRowIdx);
		final FieldVector <BigFraction> dstRow = matrix.getRowVector(dstRowIdx);
		final FieldVector <BigFraction> srcRowMul = srcRow.mapMultiply(new BigFraction(quotient));
		final FieldVector <BigFraction> dstRowSub = dstRow.subtract(srcRowMul);
		matrix.setRowVector(dstRowIdx, dstRowSub);
	}
	
	private static void swapCols(
			final FieldMatrix <BigFraction> matrix,
			final int idx1,
			final int idx2) {
		final FieldVector <BigFraction> col1 = matrix.getColumnVector(idx1);
		final FieldVector <BigFraction> col2 = matrix.getColumnVector(idx2);
		matrix.setColumnVector(idx1, col2);
		matrix.setColumnVector(idx2, col1);
	}
	
	private static void swapRows(
			final FieldMatrix <BigFraction> matrix,
			final int idx1,
			final int idx2) {
		final FieldVector <BigFraction> row1 = matrix.getRowVector(idx1);
		final FieldVector <BigFraction> row2 = matrix.getRowVector(idx2);
		matrix.setRowVector(idx1, row2);
		matrix.setRowVector(idx2, row1);
	}
	
	private static void inverseCol(final FieldMatrix <BigFraction> matrix, final int idx) {
		final FieldVector <BigFraction> col = matrix.getColumnVector(idx);
		col.mapMultiplyToSelf(BigFraction.MINUS_ONE);
		matrix.setColumnVector(idx, col);
	}
	
	private static void inverseRow(final FieldMatrix <BigFraction> matrix, final int idx) {
		final FieldVector <BigFraction> row = matrix.getRowVector(idx);
		row.mapMultiplyToSelf(BigFraction.MINUS_ONE);
		matrix.setRowVector(idx, row);
	}
	
	public static String toString(FieldMatrix <?> matrix) {
		int[] colsWidth = new int[matrix.getColumnDimension()];
		for (int row = 0; row < matrix.getRowDimension(); ++row) {
			for (int col = 0; col < matrix.getColumnDimension(); ++col) {
				colsWidth[col] = Math.max(colsWidth[col],
						String.valueOf(matrix.getEntry(row, col)).length());
			}
		}
		StringBuilder result = new StringBuilder();
		for (int row = 0; row < matrix.getRowDimension(); ++row) {
			for (int col = 0; col < matrix.getColumnDimension(); ++col) {
				result.append(String.format("%1$#" + colsWidth[col]+"s ",
						matrix.getEntry(row, col).toString()));
			}
			result.append('\n');
		}
		return result.deleteCharAt(result.length() - 1).toString();
	}
}
