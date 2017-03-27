package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.matrix.Matrix
import spire.math.Rational

trait PowerTransformationSolver[N <: MPNumber] {

  /**
   * Generate alpha-matrix given polyhedron intersection
   *
   * @param powersSeq one border per polyhedron
   */
  def generateAlphaFromTerms(powersSeqs: Seq[Seq[FracVec]]): Matrix[Rational]

  def substitute(poly: Polynomial[N], alpha: Matrix[Rational]): Polynomial[N]

  def solveShortSubstitutesSystem(substitutesSys: Polys[N]): NumVec[N]

  /** Used for transformation of variable (y to z) using solution for y */
  def varChangeFromShortSubsSolution(shortSubsSysSol: FracVec): Polys[N]
}
