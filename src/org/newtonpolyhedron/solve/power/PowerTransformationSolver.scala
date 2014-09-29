package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.entity.vector.VectorImports._

trait PowerTransformationSolver {

  /**
   * Generate alpha-matrix given polyhedron intersection
   *
   * @param termSeq one border per polyhedron
   */
  def generateAlphaFromTerms(termSeqs: Seq[Seq[Term]]): Matrix[BigFrac]

  def substitute(poly: Polynomial, alpha: Matrix[BigFrac]): Polynomial

  def solveShortSubstitutesSystem(substitutesSys: Polys): FracVec

  /** Used for transformation of variable (y to z) using solution for y */
  def varChangeFromShortSubsSolution(shortSubsSysSol: FracVec): Polys
}
