package org.newtonpolyhedron.solve.polyinter;

import java.util.List;
import java.util.Map;

import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;

public interface PolyIntersectionSolver {
	
	/**
	 * Computes a polyhedron intersections.
	 * 
	 * @param polyhedrons
	 *            source polyhedrons
	 * @param dim
	 *            polyhedron dimensions
	 * @return { vector : [ point indices list per polyhedron ] }
	 * @throws Exception
	 */
	public Map <IntVector, List <List <Integer>>> solve(
			List <List <FractionVector>> polyhedrons,
			int dim) throws Exception;
}