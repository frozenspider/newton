package org.newtonpolyhedron.solve.cone

import java.io.PrintWriter

import org.newtonpolyhedron.entity.vector.VectorImports._

trait ConeSolver {
  /**
   * Returns a fundamental solution of a linear inequations system
   *
   * @param inequations
   *            list of linear inequations coefficients.
   * @param basis
   *            a basis to solve for (or {@code null})
   * @param spaceDimension
   *            target space dimension
   * @param output
   *            an output for intermediate values (final result won't be printed anyway)
   * @return a fundamental solution for an linear inequation system
   * @throws InterruptedException
   *             if execution was interrupted
   */
  def solve(inequations: Seq[IntVec],
            basis: Seq[IntVec],
            spaceDimension: Int,
            output: PrintWriter): Seq[IntVec]
}