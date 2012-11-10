package org.newtonpolyhedron.solve.polyinter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.fs.utils.ListUtils;
import org.fs.utils.collection.iter.AbstractIterator;
import org.fs.utils.collection.map.BasicSortedMap;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.cone.ConeSolver;
import org.newtonpolyhedron.utils.ArithUtils;
import org.newtonpolyhedron.utils.NullPrintWriter;
import org.newtonpolyhedron.utils.PointUtils;

public class PolyIntersectionSolverImpl implements PolyIntersectionSolver {
	
	private final ConeSolver	coneSolver;
	
	public PolyIntersectionSolverImpl(final ConeSolver coneSolver) {
		this.coneSolver = coneSolver;
	}
	
	@Override
	public Map <IntVector, List <List <Integer>>> solve(
			final List <List <FractionVector>> polyhedrons,
			final int dim) throws Exception {
		if (dim < 3)
			throw new IllegalArgumentException("No intersections are possible below 3-dimension");
		if (polyhedrons.size() < dim - 1)
			throw new IllegalArgumentException(
					"Not enough polyhedrons for intersecting at dimension " + dim);
		final List <Integer> polyPtsCount = new ArrayList <Integer>();
		for (final List <FractionVector> poly : polyhedrons) {
			polyPtsCount.add(poly.size());
		}
		
		final Map <IntVector, List <List <Integer>>> ptsForVectors = new BasicSortedMap <IntVector, List <List <Integer>>>();
		final int polyCount = polyhedrons.size();
		
		for (final List <Integer> indices : new IndicesIterable(polyCount, polyPtsCount)) {
			
			// ++ Construct a system
			final List <List <IntVector>> eqSystems = new ArrayList <List <IntVector>>();
			for (int i = 0; i < polyCount; ++i) {
				eqSystems.add(PointUtils.copySubtractPointAsInt(polyhedrons.get(i), indices.get(i)));
			}
			final List <IntVector> commonEqSys = new ArrayList <IntVector>();
			for (final List <IntVector> eqSys : eqSystems) {
				commonEqSys.addAll(eqSys);
			}
			// -- Construct a system
			
			final List <IntVector> solution = coneSolver.solve(commonEqSys, null, dim,
					NullPrintWriter.instance);
			
			removeNonIntersectingSolutions(solution, eqSystems);
			
			for (final IntVector solutionVector : solution) {
				List <List <Integer>> list = ptsForVectors.get(solutionVector);
				if (list == null) {
					list = new ArrayList <List <Integer>>();
					ptsForVectors.put(solutionVector, list);
				}
				list.add(indices);
			}
		}
		return ptsForVectors;
	}
	
	protected static void removeNonIntersectingSolutions(
			final List <IntVector> solution,
			final List <List <IntVector>> equationSystems) {
		for (final Iterator <IntVector> solutionIter = solution.iterator(); solutionIter.hasNext();) {
			final IntVector currSolution = solutionIter.next();
			for (final List <IntVector> currEqSystem : equationSystems) {
				boolean nullifiesAtLeastOne = false;
				for (final IntVector eq : currEqSystem) {
					if (ArithUtils.isZero(eq.dotProduct(currSolution))) {
						nullifiesAtLeastOne = true;
						break;
					}
				}
				if (!nullifiesAtLeastOne) {
					solutionIter.remove();
					break;
				}
			}
		}
	}
	
	//
	// Inner classes
	//
	private static class IndicesIterable implements Iterable <List <Integer>> {
		
		private final int				length;
		private final List <Integer>	maximums;
		
		public IndicesIterable(final int length, final List <Integer> maximums) {
			this.length = length;
			this.maximums = maximums;
			
		}
		
		@Override
		public Iterator <List <Integer>> iterator() {
			return new IndicesIterator(length);
		}
		
		private class IndicesIterator extends AbstractIterator <List <Integer>> {
			
			private List <Integer>	current;
			private List <Integer>	next;
			
			public IndicesIterator(final int length) {
				current = new ArrayList <Integer>(//
						Collections.nCopies(//
								length,//
								Integer.valueOf(0)//
						)//
				);
				current.set(length - 1, -1);
			}
			
			@Override
			public synchronized boolean hasNext() {
				if (next != null) return true;
				doGetNext();
				return next != null;
			}
			
			@Override
			public synchronized List <Integer> next() {
				if (!hasNext()) throw new NoSuchElementException();
				current = next;
				next = null;
				return new ArrayList <Integer>(current);
			}
			
			private void doGetNext() {
				next = new ArrayList <Integer>(current);
				int idx = next.size() - 1;
				ListUtils.inc(next, idx);
				while (idx > 0) {
					if (next.get(idx) < maximums.get(idx)) {
						break;
					}
					next.set(idx, 0);
					--idx;
					ListUtils.inc(next, idx);
				}
				if (next.get(0) >= maximums.get(0)) {
					next = null;
				}
			}
		}
	}
}
