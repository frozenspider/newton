package org.newtonpolyhedron.solve.surface

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.vector.VectorImports._

trait SurfaceBuilder {
  def surfaces(lookupTable: KeyTable[IntVec, Int, Boolean], dim: Int): Map[Int, SortedSet[Surface]]
}
