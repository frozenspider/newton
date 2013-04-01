package org.newtonpolyhedron.solve.polyinter
import org.newtonpolyhedron.entity.vector.FracMathVec

import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntVector

trait PolyIntersectionSolver {
  /**
   * Computes a polyhedron intersections.
   *
   * @param polyhedrons
   *            source polyhedrons
   * @param dim
   *            polyhedron dimensions
   * @return { vector : [ point indices list per polyhedron ] }
   * @throws Exception
   */
  def solve(polyhedrons: java.util.List[java.util.List[FractionVector]],
            dim: Int): java.util.Map[IntVector, java.util.List[java.util.List[Integer]]]

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