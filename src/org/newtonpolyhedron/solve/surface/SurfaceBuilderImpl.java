package org.newtonpolyhedron.solve.surface;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.fs.utils.collection.list.SortedArrayList;
import org.fs.utils.collection.set.IndexedSet;
import org.fs.utils.collection.table.KeyTable;
import org.newtonpolyhedron.entity.Surface;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.utils.ListCombinationIterator;

public class SurfaceBuilderImpl implements SurfaceBuilder {
	
	@Override
	public Map <Integer, IndexedSet <Surface>> getSurfaces(
			final KeyTable <IntVector, Integer, Boolean> lookupTable,
			final int dim) {
		final Map <Integer, IndexedSet <Surface>> surfacesMap = new LinkedHashMap <Integer, IndexedSet <Surface>>();
		final List <List <Integer>> lookupTableData = extactLookupTableData(lookupTable);
		
		IndexedSet <Surface> surfacesSet = new SortedArrayList <Surface>();
		for (int targetDim = dim - 1; targetDim >= 0; --targetDim) {
			surfacesSet = findCommonSurfaces(dim, targetDim, surfacesSet, lookupTableData);
			surfacesMap.put(targetDim, surfacesSet);
		}
		return surfacesMap;
	}
	
	private List <List <Integer>> extactLookupTableData(final KeyTable <IntVector, Integer, Boolean> lookupTable) {
		final List <List <Integer>> result = new ArrayList <List <Integer>>();
		for (final IntVector point : lookupTable.rowKeyList()) {
			final List <Integer> row = new ArrayList <Integer>();
			for (final int colKey : lookupTable.colKeyList()) {
				if (lookupTable.get(point, colKey) == Boolean.TRUE) {
					row.add(colKey);
				}
			}
			result.add(row);
		}
		return result;
	}
	
	/**
	 * Finds the common surfaces of a lesser dimension
	 * 
	 * @param polyhedronDimension
	 *            polyhedron dimension
	 * @param targetDimension
	 *            a target dimension
	 * @param upperLevelSurfaces
	 *            surfaces of an upper level
	 * @param lookupTableData
	 *            data from lookup table (i.e. table without keys)
	 * @return list of surfaces of a lower dimension
	 */
	protected static IndexedSet <Surface> findCommonSurfaces(
			final int polyhedronDimension,
			final int targetDimension,
			final Collection <Surface> upperLevelSurfaces,
			final List <List <Integer>> lookupTableData) {
		if (polyhedronDimension < 2) throw new IllegalArgumentException("Polyhedron dimension >= 2");
		if (targetDimension < 0) throw new IllegalArgumentException("Target dimension must be nonnegative");
		if (targetDimension >= polyhedronDimension)
			throw new IllegalArgumentException("Target dimension must be <= poly dimension");
		final int width = polyhedronDimension - targetDimension;
		
		final ListCombinationIterator <List <Integer>> iter = new ListCombinationIterator <List <Integer>>(
				lookupTableData, width);
		final IndexedSet <Surface> surfaces = new SortedArrayList <Surface>();
		
		//
		// Forming naked unchecked surfaces
		//
		// List may contains semi-duplicates and nothing is yet known
		// about higher-dimension surface associations
		//
		while (iter.hasNext()) {
			final List <List <Integer>> vectorCombination = iter.next();
			final List <Integer> commonPoints = new ArrayList <Integer>();
			boolean justStarted = true;
			for (final List <Integer> pointList : vectorCombination) {
				if (justStarted) {
					justStarted = false;
					commonPoints.addAll(pointList);
				} else {
					commonPoints.retainAll(pointList);
				}
			}
			if (targetDimension > 0) {
				// Common surface
				if (commonPoints.size() > targetDimension) {
					surfaces.add(new Surface(commonPoints, null));
				}
			} else {
				// Vertex
				for (final Integer commonPoint : commonPoints) {
					surfaces.add(new Surface(Arrays.asList(commonPoint), null));
				}
			}
		}
		
		removeSemiDuplicates(surfaces);
		
		gatherHigherLevelSurfacesInfo(surfaces, upperLevelSurfaces, targetDimension, polyhedronDimension);
		
		return surfaces;
	}
	
	private static void removeSemiDuplicates(final Collection <Surface> surfaces) {
		for (final Iterator <Surface> resultIter = surfaces.iterator(); resultIter.hasNext();) {
			final Surface currentSurface = resultIter.next();
			
			// A list, containing all surfaces but current
			final List <Surface> surfaceListCopy = new ArrayList <Surface>(surfaces);
			surfaceListCopy.remove(currentSurface);
			
			if (surfacesConatinsGiven(currentSurface, surfaceListCopy)) {
				resultIter.remove();
			}
		}
	}
	
	private static void gatherHigherLevelSurfacesInfo(
			final Collection <Surface> surfaces,
			final Collection <Surface> upperLevelSurfaces,
			final int targetDimension,
			final int polyhedronDimension) {
		for (final Iterator <Surface> resultIter = surfaces.iterator(); resultIter.hasNext();) {
			final Surface currentSurface = resultIter.next();
			
			final List <Surface> superior = surfacesConatiningGiven(currentSurface, upperLevelSurfaces);
			
			if (targetDimension == 0 && superior.size() < polyhedronDimension - 1) {
				resultIter.remove();
			} else {
				currentSurface.addUpperDimSurfaces(superior);
			}
		}
	}
	
	/**
	 * Checks, whether the list already contains a supersurface, which is superior to the given
	 * surface (in other words, contains the given surface as a subsurface). If <code>true</code>,
	 * child surface should not be added to a list (as it is indirectly there).
	 * <p>
	 * Example:
	 * <p>
	 * Surface <code>A = {1,2,4,6}</code> is superior to the surface <code>B = {1,4,6}</code>, as
	 * {@code A} contains all points of {@code B}
	 * 
	 * @param upperLevelSurfaces
	 *            list of known surfaces
	 * @param surface
	 *            new surface to test
	 * @return <code>true</code>, if list contains a surface, which is superior to the given.
	 * @see #surfacesConatiningGiven(Surface, Collection)
	 */
	private static boolean surfacesConatinsGiven(final Surface surface, final Collection <Surface> upperLevelSurfaces) {
		final List <Integer> pts = surface.getPointIdxList();
		for (final Surface supersurface : upperLevelSurfaces) {
			if (supersurface.getPointIdxList().containsAll(pts)) return true;
		}
		return false;
	}
	
	/**
	 * Gather surfaces superior to the given surface. For more information, see
	 * {@link #surfacesConatinsGiven(List, Surface)}.
	 * <p>
	 * The only difference is a result - this method returns a list of superior surface indices
	 * rather than just a boolean.
	 * 
	 * @param upperLevelSurfaces
	 *            list of known surfaces
	 * @param surface
	 *            new surface to test
	 * @return list of superior surfaces
	 * @see #surfacesConatinsGiven(Surface, Collection)
	 */
	private static List <Surface> surfacesConatiningGiven(
			final Surface surface,
			final Collection <Surface> upperLevelSurfaces) {
		final List <Integer> pts = surface.getPointIdxList();
		final List <Surface> surfacesList = new ArrayList <Surface>();
		for (final Surface supersurface : upperLevelSurfaces) {
			if (supersurface.getPointIdxList().containsAll(pts)) {
				surfacesList.add(supersurface);
			}
		}
		return surfacesList;
	}
}
