package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron.Polynomial
import org.newtonpolyhedron.Polys
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.vector.FracMathVec

trait PowerTransformationSolver {

  /**
   * Generate alpha-matrix given polyhedron intersection
   *
   * @param powersSeq one border per polyhedron
   */
  def generateAlphaFromTerms(powersSeqs: Seq[Seq[FracMathVec]]): Matrix[BigFrac]

  def substitute(poly: Polynomial, alpha: Matrix[BigFrac]): Polynomial

  def solveShortSubstitutesSystem(substitutesSys: Polys): FracMathVec

  /** Used for transformation of variable (y to z) using solution for y */
  def varChangeFromShortSubsSolution(shortSubsSysSol: FracMathVec): Polys
}