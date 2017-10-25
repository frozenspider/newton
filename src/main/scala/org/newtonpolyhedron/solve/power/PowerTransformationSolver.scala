package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron.NewtonImports._
import spire.math.Rational

trait PowerTransformationSolver[N <: MPNumber, M <: MPMatrix] {

  /**
   * Generate alpha-matrix given polyhedron intersection
   *
   * @param powersSeq one border per polyhedron
   */
  def generateAlphaFromTerms(powersSeqs: Seq[Seq[NumVec[N]]]): M

  def substitute(poly: Polynomial[N], alpha: M): Polynomial[N]

  def solveShortSubstitutesSystem(substitutesSys: Polys[N]): NumVec[N]

  /** Used for transformation of variable (y to z) using solution for y */
  def varChangeFromShortSubsSolution(shortSubsSysSol: NumVec[N]): Polys[N]
}
