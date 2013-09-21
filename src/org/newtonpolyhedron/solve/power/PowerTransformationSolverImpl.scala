package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.PolynomialWrapper._
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import org.newtonpolyhedron.solve.eqsys.EqSystemSolver
import org.newtonpolyhedron.solve.changevars.ChangerOfVariables
import org.newtonpolyhedron.entity.Product

class PowerTransformationSolverImpl(
  val umm: UnimodularMatrixMaker,
  val eqSysSolver: EqSystemSolver)
    extends PowerTransformationSolver {

  private type Powers = IndexedSeq[FracMathVec]

  private def vec = FracMathVec
  private def vecf(s: Seq[BigFrac]) = vec(s: _*)

  def alphaFromTermSubtractionPairs(termPairs: Seq[(Term, Term)]): Matrix[BigFrac] = {
    val dim = termPairs.size + 1
    require(termPairs forall {
      case (term1, term2) => term1.powers.dim == dim && term2.powers.dim == dim
    }, "Each term should have the same dimension: number of pairs + 1")
    val matrixBase = (termPairs map { case (term1, term2) => term1.powers - term2.powers }) :+ vec.zero(3)
    val matrix = Matrix[BigFrac, FracMathVec](matrixBase)
    val alpha = umm unimodularFrom matrix
    assert(alpha.elementsByRow map (_._3) forall (v => v.den == 1))
    alpha
  }

  //
  // =========================
  //
  private def matrixByRows(m: Matrix[BigFrac]): Seq[FracMathVec] =
    m.elementsByRow map (_._3) grouped (m.colNum) map vecf toIndexedSeq

  private def inverse(m: Matrix[BigFrac]): Matrix[BigFrac] = {
    val inv = m.inv
    assert(inv.elementsByRow map (_._3) forall (_.den == 1))
    inv
  }

  private def getLowestPowers(powers: Powers) =
    vecf(powers map (_.elements) reduce ((_, _).zipped map (_ min _)))

  def substitute(poly: Polynomial, alpha: Matrix[BigFrac]): Polynomial = {
    require(alpha.isSquare, "Alpha matrix should be square")
    val alphaInvByRows = matrixByRows(inverse(alpha))
    val rawPows =
      poly.powers map (p => (alphaInvByRows, p.elements).zipped map ((alphaRow, polyPow) => alphaRow * polyPow) reduce (_ + _))
    val lowestPows = getLowestPowers(rawPows)
    val resPows = rawPows map (_ - lowestPows)
    //    println(" === Alpha Inv:       " + alphaInvByRows)
    //    println(" === Unreduced:       " + rawPows)
    //    println(" === Lowest powers:   " + lowestPows)
    poly.coeffs zip resPows map Term.apply
  }

  //
  // =========================
  //
  def solveShortSubstitutesSystem(simpleSys: Polys): FracMathVec = {
    require(simpleSys forall (_ forall (t => t.powers.elements.last == 0)))
    // Remove last (zero) component
    val truncatedSimpleSys = simpleSys map (_ map (_.mapPowers(_.elements.dropRight(1))))
    val sol = eqSysSolver.solve(truncatedSimpleSys)
    assert(sol.tail.isEmpty)
    // Add it back
    sol.head.elements :+ BigFrac.ZERO
  }

  //
  // =========================
  //
  def varChangeFromShortSubsSolution(vecs: FracMathVec): Polys = {
    val dim = vecs.dim
    val zeroVec = vec.zero(dim)
    def zeroOneVec(i: Int) = zeroVec updated (i, 1)
    // 0,0,0  1,0,0
    // 0,0,0  0,1,0
    // 0,0,0  0,0,1
    val one = Product.ONE
    val zPows = (0 until dim) map (i => IndexedSeq(zeroVec, zeroOneVec(i)))
    val coeffRows = vecs.elements map (solVal => IndexedSeq(Product(solVal), one))
    val tPolys = (coeffRows, zPows).zipped map ((coeffs, zRows) => (coeffs zip zRows) map Term.apply)
    tPolys map (_.skipZeroTerms)
  }
}