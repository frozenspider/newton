package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.utils.PolynomialUtils.Polys

/**
 * Solver for system of polynomial equations
 */
trait EqSystemSolver {

  /** Checks whether or not the system can be solved by this solver */
  def canSolve(system: Polys): Boolean = whyCantSolve(system).isEmpty

  /** Returns a user-friendly reason why system can't be solved, or None if it can */
  def whyCantSolve(system: Polys): Option[String]

  /** Solves the given equation system */
  def solve(system: Polys): Seq[FracVec]
}
