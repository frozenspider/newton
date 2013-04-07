package org.newtonpolyhedron.solve.polyinter

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
   * @return { vector : [ point indices list per polyhedron ] }
   */
  def solve(polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
            dim: Int): Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]]
}