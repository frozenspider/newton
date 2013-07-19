package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.Polynomial
import org.newtonpolyhedron.Polys
import org.newtonpolyhedron.entity.vector.FracMathVec

trait SimpleEqSystemSolver {
  /**
   * Is only capable of solving *simple* equation systems.
   * <p>
   * "Simple" here means that each equation should be a polynomial consisting of exactly two terms.
   */
  def solveSimpleEqSys(system: Polys): FracMathVec
}