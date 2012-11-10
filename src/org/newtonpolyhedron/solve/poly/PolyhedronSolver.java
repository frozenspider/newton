package org.newtonpolyhedron.solve.poly;

import java.io.PrintWriter;
import java.util.List;

import org.fs.utils.collection.table.KeyTable;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;

public interface PolyhedronSolver {
	
	public KeyTable <IntVector, Integer, Boolean> solve(
			List <FractionVector> points,
			List <IntVector> commonLimits,
			List <IntVector> wishfulBasis,
			PrintWriter output) throws Exception;
}