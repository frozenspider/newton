package org.newtonpolyhedron.solve.polyinter
import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec

trait PolyIntersectionSolver {
  /**
   * Computes a polyhedron intersections.
   *
   * @param polyhedrons
   *            source polyhedrons
   * @param dim
   *            polyhedron dimensions
   * @return { polyIdx, vector -> [ points giving this vector for this poly when intersecting ] }
   */
  def solve(polyhedrons: Seq[IndexedSeq[FracMathVec]],
            dim: Int): KeyTable[Int, IntMathVec, SortedSet[Int]]
}