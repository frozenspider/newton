package org.newtonpolyhedron.solve.face

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.entity.Face
import org.newtonpolyhedron.entity.vector.VectorImports._

trait FaceBuilder {
  def faces(lookupTable: KeyTable[IntVec, Int, Boolean], dim: Int): Map[Int, SortedSet[Face]]
}
