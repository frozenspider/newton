package org.newtonpolyhedron.solve.polyinter

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.NewtonImports._

trait PolyIntersectionSolver[N <: MPNumber] {
  /**
   * Computes a polyhedron intersections.
   *
   * @param polyhedrons
   *            source polyhedrons
   * @param dim
   *            polyhedron dimensions
   * @return { polyIdx, vector -> [ points giving this vector for this poly when intersecting ] }
   */
  def solve(
      polyhedrons: Seq[IndexedSeq[NumVec[N]]],
      dim:         Int
  ): KeyTable[Int, IntVec, SortedSet[Int]]
}
