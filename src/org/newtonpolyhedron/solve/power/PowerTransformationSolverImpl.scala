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

  override def generateAlphaFromTerms(termSeqs: Seq[Seq[Term]]): (Matrix[BigFrac], IndexedSeq[(Term, Term)]) = {
    val dim = termSeqs.size + 1
    require(termSeqs forall (_ forall (_.powers.dim == dim)), "Each term should have the same dimension: number of pairs + 1")
    def pairsStream = choosePairs(termSeqs)
    val pairsWithAlphasStream: Stream[(Matrix[BigFrac], Seq[(Term, Term)])] =
      pairsStream map { pairs =>
        val matrixBase = (pairs map {
          case (term1, term2) => term1.powers - term2.powers
        }) :+ vec.zero(dim)
        val matrix = Matrix[BigFrac, FracMathVec](matrixBase)
        val alpha = umm unimodularFrom matrix
        assert(alpha.elementsByRow map (_._3) forall (v => v.den == 1))
        (alpha, pairs)
      }
    val res = pairsWithAlphasStream find {
      case (alpha, pairs) => alpha.det == 1
    }
    val (alpha, pairs) = res.get
    (alpha, pairs.toIndexedSeq)
  }

  def choosePairs[T](termSeqs: Seq[Seq[T]]): Stream[Seq[(T, T)]] = {
    def recursion(input: Seq[Stream[(T, T)]]): Stream[Seq[(T, T)]] = {
      if (input.tail.isEmpty) {
        // Base case
        input.head map (stream => Seq(stream))
      } else {
        // Induction step
        val rest = recursion(input.tail)
        val r = for {
          x <- input.head
          xs <- rest
        } yield x +: xs
        r
      }
    }
    def slidingPairs[A](s: Seq[A]): Stream[(A, A)] = {
      s.sliding(2).toStream map (seq => (seq.head, seq.tail.head))
    }
    val pairsStream = termSeqs map slidingPairs
    recursion(pairsStream)
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

  override def substitute(poly: Polynomial, alpha: Matrix[BigFrac]): Polynomial = {
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
  override def solveShortSubstitutesSystem(simpleSys: Polys): FracMathVec = {
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
  override def varChangeFromShortSubsSolution(vecs: FracMathVec): Polys = {
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