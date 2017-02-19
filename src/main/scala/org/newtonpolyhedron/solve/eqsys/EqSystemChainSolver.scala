package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.utils.PolynomialUtils._

class EqSystemChainSolver(solversChain: Seq[EqSystemSolver]) extends EqSystemSolver {

  override def whyCantSolve(system: Polys): Option[String] = None

  def solve(system: Polys): Seq[FracVec] = {
    val solutionOption = solversChain collectFirst {
      case solver if solver canSolve system => solver solve system
    }
    assert(solutionOption.isDefined,
      "Chain solver for system of equations was unable to solve provided system! "
        + "That's a bug, at one solver in chain must be manual and omniscient")
    solutionOption.get
  }
}
