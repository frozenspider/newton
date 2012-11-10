package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.fs.utils.collection.table.ArrayListTable;
import org.fs.utils.collection.table.Table;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker;

public class UnimodularMatrixMakerPrinter extends SolverPrinter <UnimodularMatrixMaker> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	
	public UnimodularMatrixMakerPrinter(
			final UnimodularMatrixMaker solver,
			final FieldMatrix <BigFraction> baseMatrix,
			final PrintWriter output) {
		super(solver, output);
		this.baseMatrix = baseMatrix;
	}
	
	@Override
	protected void solveFor(final UnimodularMatrixMaker solver, final PrintWriter output)
			throws Exception {
		final FieldMatrix <BigFraction> alpha = solver.getUnimodularFrom(baseMatrix);
		output.println();
		output.println("Alpha-matrix: ");
		
		final Table <BigFraction> table = new ArrayListTable <BigFraction>();
		for (int i = 0; i < alpha.getRowDimension(); i++) {
			for (int j = 0; j < alpha.getColumnDimension(); j++) {
				table.put(i, j, alpha.getEntry(i, j));
			}
		}
		output.println(table);
	}
}
