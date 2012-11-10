package org.newtonpolyhedron.solverprinters;

import java.io.PrintWriter;
import java.math.BigInteger;
import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.solve.matrixminorgcd.MatrizMinorGCDSolver;

public class MatrizMinorGCDSolverPrinter extends SolverPrinter <MatrizMinorGCDSolver> {
	
	private final FieldMatrix <BigFraction>	baseMatrix;
	
	public MatrizMinorGCDSolverPrinter(
			final MatrizMinorGCDSolver solver,
			final FieldMatrix <BigFraction> baseMatrix,
			final PrintWriter output) {
		super(solver, output);
		this.baseMatrix = baseMatrix;
	}
	
	@Override
	protected void solveFor(final MatrizMinorGCDSolver solver, final PrintWriter output)
			throws Exception {
		final Pair <Integer, List <BigInteger>> result = solver.getLastRowGcd(baseMatrix);
		output.println();
		output.println("Last row minors: ");
		output.println(result.getRight());
		output.println("Minors GCD: ");
		output.println(result.getLeft());
	}
}
