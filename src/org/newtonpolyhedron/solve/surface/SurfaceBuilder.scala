package org.newtonpolyhedron.solve.surface
import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.vector.IntMathVec

trait SurfaceBuilder {
  def surfaces(lookupTable: KeyTable[IntMathVec, Int, Boolean], dim: Int): Map[Int, SortedSet[Surface]]
}