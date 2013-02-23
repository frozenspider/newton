package org.newtonpolyhedron.solve.surface;

import java.util.Map;

import org.fs.utils.collection.set.IndexedSet;
import org.fs.utils.collection.table.KeyTable;
import org.newtonpolyhedron.entity.Surface;
import org.newtonpolyhedron.entity.vector.IntVector;

public interface SurfaceBuilder {
	
	public Map <Integer, IndexedSet <Surface>> getSurfaces(KeyTable <IntVector, Integer, Boolean> lookupTable, int dim);
	
}