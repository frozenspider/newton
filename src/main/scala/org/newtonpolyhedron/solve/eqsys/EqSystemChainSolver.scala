package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.NewtonImports._

class EqSystemChainSolver[N <: MPNumber](solversChain: Seq[EqSystemSolver[N]]) extends EqSystemSolver[N] {

  override def whyCantSolve(system: Polys[N]): Option[String] = None

  override def solve(system: Polys[N]): Seq[NumVec[N]] = {
    val solutionOption = solversChain collectFirst {
      case solver if solver canSolve system => solver solve system
    }
    assert(
      solutionOption.isDefined,
      "Chain solver for system of equations was unable to solve provided system! "
        + "That's a bug, as one solver in chain must be manual and omniscient"
    )
    solutionOption.get
  }
}
