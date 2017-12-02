package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.NewtonImports._

/**
 * Solver for system of polynomial equations
 */
trait EqSystemSolver[N <: MPNumber] {

  /** Checks whether or not the system can be solved by this solver */
  def canSolve(system: Polys[N]): Boolean = whyCantSolve(system).isEmpty

  /** Returns a user-friendly reason why system can't be solved, or None if it can */
  def whyCantSolve(system: Polys[N]): Option[String]

  /** Solves the given equation system */
  def solve(system: Polys[N]): Seq[NumVec[N]]
}
