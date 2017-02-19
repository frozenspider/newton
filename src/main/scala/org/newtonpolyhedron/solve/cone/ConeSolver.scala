package org.newtonpolyhedron.solve.cone

import org.newtonpolyhedron.entity.vector.VectorImports._

trait ConeSolver {
  /**
   * Returns a fundamental solution of a linear inequations system
   *
   * @param inequations
   *            list of linear inequations coefficients.
   * @param basisOption
   *            a basis to solve for
   * @param spaceDimension
   *            target space dimension
   * @return a fundamental solution for an linear inequation system
   * @throws InterruptedException
   *             if execution was interrupted
   */
  def solve(inequations: Seq[IntVec],
            basisOption: Option[Seq[IntVec]],
            spaceDimension: Int): Seq[IntVec]
}
