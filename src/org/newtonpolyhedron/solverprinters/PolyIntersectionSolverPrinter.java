package org.newtonpolyhedron.solverprinters;

import static java.text.MessageFormat.*;

import java.io.PrintWriter;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.fs.utils.collection.list.SortedArrayList;
import org.fs.utils.collection.table.ArrayListKeyTable;
import org.fs.utils.collection.table.KeyTable;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver;

public class PolyIntersectionSolverPrinter extends SolverPrinter <PolyIntersectionSolver> {
	
	private final List <List <FractionVector>>	polyhedrons;
	private final int							dim;
	
	public PolyIntersectionSolverPrinter(
			final PolyIntersectionSolver solver,
			final List <List <FractionVector>> polyhedrons,
			final int dim,
			final PrintWriter output) {
		super(solver, output);
		this.polyhedrons = polyhedrons;
		this.dim = dim;
	}
	
	@Override
	public void solveFor(final PolyIntersectionSolver solver, final PrintWriter output) {
		output.println(title("Polyhedron intersection"));
		output.println(header("Original points:"));
		for (int i = 0; i < polyhedrons.size(); ++i) {
			output.println(subheader("Poly " + i));
			final List <FractionVector> points = polyhedrons.get(i);
			for (int j = 0; j < points.size(); j++) {
				output.println(format(" Q{0} = {1}", j, points.get(j)));
			}
		}
		
		final Map <IntVector, List <List <Integer>>> ptsForVectors = solver.solve(polyhedrons, dim);
		
		final KeyTable <Integer, IntVector, Set <Integer>> vectorPointTable = reverseTableMeaning(ptsForVectors);
		output.println(vectorPointTable);
	}
	
	private KeyTable <Integer, IntVector, Set <Integer>> reverseTableMeaning(
			final Map <IntVector, List <List <Integer>>> ptsForVectors) {
		final KeyTable <Integer, IntVector, Set <Integer>> vectPtTable = new ArrayListKeyTable <Integer, IntVector, Set <Integer>>();
		for (final Entry <IntVector, List <List <Integer>>> entry : ptsForVectors.entrySet()) {
			final IntVector vector = entry.getKey();
			for (final List <Integer> indices : entry.getValue()) {
				for (int i = 0; i < indices.size(); ++i) {
					final Set <Integer> pts = vectPtTable.get(i, vector, new SortedArrayList <Integer>());
					pts.add(indices.get(i));
				}
			}
		}
		return vectPtTable;
	}
}
