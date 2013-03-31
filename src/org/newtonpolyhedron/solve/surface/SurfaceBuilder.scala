package org.newtonpolyhedron.solve.surface

import org.fs.utils.collection.set.IndexedSet
import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntVector

trait SurfaceBuilder {
  def getSurfaces(lookupTable: KeyTable[IntVector, Integer, java.lang.Boolean], dim: Int): java.util.Map[Integer, IndexedSet[Surface]]

  def surfaces(lookupTable: KeyTable[IntMathVec, Int, Boolean], dim: Int): Map[Int, Set[Surface]]
}