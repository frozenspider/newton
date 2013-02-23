package org.newtonpolyhedron.solve.poly;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

import org.fs.utils.collection.table.ArrayListKeyTable;
import org.fs.utils.collection.table.KeyTable;
import org.fs.utils.collection.table.KeyTables;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.cone.ConeSolver;
import org.newtonpolyhedron.utils.NullPrintWriter;
import org.newtonpolyhedron.utils.PointUtils;

/** @author Alexander "FS" Abdugafarov */
public class PolyMotzkinBurgerSolver implements PolyhedronSolver {
	
	private final ConeSolver	coneSolver;
	
	public PolyMotzkinBurgerSolver(final ConeSolver coneSolver) {
		this.coneSolver = coneSolver;
	}
	
	@Override
	public KeyTable <IntVector, Integer, Boolean> solve(
			final List <FractionVector> points,
			final List <IntVector> commonLimits,
			final List <IntVector> wishfulBasis,
			final PrintWriter output) throws Exception {
		final int dim = points.get(0).getDim();
		
		final KeyTable <IntVector, Integer, Boolean> lookupTable = new ArrayListKeyTable <IntVector, Integer, Boolean>();
		fillTableIdxKeys(lookupTable, points.size());
		
		final List <IntVector> normalVectorList = new ArrayList <IntVector>();
		
		for (int currPtIdx = 0; currPtIdx < points.size(); ++currPtIdx) {
			// Forming a list of equations by substracting current point
			final List <IntVector> pointless = PointUtils.copySubtractPointAsInt(points, currPtIdx);
			
			// Adding a common limits - if any
			if (commonLimits != null) {
				pointless.addAll(commonLimits);
			}
			
			final List <IntVector> tempSols = coneSolver.solve(pointless, wishfulBasis, dim, NullPrintWriter.instance);
			
			for (final IntVector tempSol : tempSols) {
				boolean alreadyThere = false;
				for (final IntVector normalVec : normalVectorList) {
					if (normalVec.equals(tempSol)) {
						lookupTable.put(normalVec, currPtIdx, true);
						alreadyThere = true;
						break;
					}
				}
				if (!alreadyThere) {
					normalVectorList.add(tempSol);
					lookupTable.put(tempSol, currPtIdx, true);
				}
			}
		}
		
		// lookupTableSort(normalVectorList, lookupTable, false);
		KeyTables.sortByRowHeaders(lookupTable, true);
		KeyTables.sortByColHeaders(lookupTable, true);
		
		return lookupTable;
	}
	
	private void fillTableIdxKeys(final KeyTable <IntVector, Integer, Boolean> lookupTable, final int upTo) {
		for (int i = 0; i < upTo; ++i) {
			lookupTable.put(null, i, null);
		}
		lookupTable.removeRow(null);
	}
}
