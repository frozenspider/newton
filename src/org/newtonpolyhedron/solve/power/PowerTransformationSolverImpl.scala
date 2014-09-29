package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.PolynomialWrapper._
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import org.newtonpolyhedron.solve.eqsys.EqSystemSolver
import org.newtonpolyhedron.solve.changevars.ChangerOfVariables
import org.newtonpolyhedron.entity.Product

class PowerTransformationSolverImpl(
  val umm: UnimodularMatrixMaker,
  val eqSysSolver: EqSystemSolver)
    extends PowerTransformationSolver {

  private type Powers = Seq[FracVec]
  private type Coeffs = Seq[Product]

  private def vecf(s: Seq[BigFrac]) = FracVec(s: _*)

  override def generateAlphaFromTerms(termSeqs: Seq[Seq[Term]]): Matrix[BigFrac] = {
    // TODO: Make sure this works for more than 2 polys
    val dim = termSeqs.size + 1
    def lastRowMinors(m: Matrix[BigFrac]): Seq[BigFrac] = {
      (0 until m.colCount) map { c => m.minor(m.rowCount - 1, c) }
    }
    require(termSeqs forall (_ forall (_.powers.size == dim)), "Each term should have the same dimension: number of pairs + 1")
    def pairsStream: Stream[Seq[(Term, Term)]] = choosePairs(termSeqs)
    val alphasStream: Stream[Matrix[BigFrac]] =
      pairsStream map { pairs =>
        val matrixBase = (pairs map {
          case (term1, term2) => term1.powers - term2.powers
        }) :+ FracVec.zero(dim)
        Matrix.fromVectors(matrixBase)
      } filter { matrix =>
        !lastRowMinors(matrix).contains(0)
      } map { matrix =>
        val alpha = umm unimodularFrom matrix
        assert(alpha.elementsByRow map (_._3) forall (v => v.den == 1))
        assert(alpha.det == 1)
        alpha
      }
    alphasStream.head
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
  private def matrixByRows(m: Matrix[BigFrac]): Seq[FracVec] =
    m.elementsByRow map (_._3) grouped (m.colCount) map vecf toIndexedSeq

  private def inverse(m: Matrix[BigFrac]): Matrix[BigFrac] = {
    val inv = m.inv
    assert(inv.elementsByRow map (_._3) forall (_.den == 1))
    inv
  }

  private def getLowestPowers(powers: Powers) =
    vecf(powers reduce ((_, _).zipped map (_ min _)))

  override def substitute(poly: Polynomial, alpha: Matrix[BigFrac]): Polynomial = {
    require(alpha.isSquare, "Alpha matrix should be square")
    val alphaInvByRows = matrixByRows(inverse(alpha))
    val rawPows =
      poly.powers map (p => (alphaInvByRows, p).zipped map ((alphaRow, polyPow) => alphaRow * polyPow) reduce (_ + _))
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
  override def solveShortSubstitutesSystem(simpleSys: Polys): FracVec = {
    require(simpleSys forall (_ forall (t => t.powers.last == 0)))
    // Remove last (zero) component
    val truncatedSimpleSys: Polys = simpleSys map (_ map (_.mapPowers(_ dropRight 1)))
    val sol: Powers = eqSysSolver.solve(truncatedSimpleSys)
    assert(sol.tail.isEmpty)
    // Add it back
    sol.head :+ BigFrac.ZERO
  }

  //
  // =========================
  //
  override def varChangeFromShortSubsSolution(vecs: FracVec): Polys = {
    val dimension = vecs.size
    val zeroVec = FracVec.zero(dimension)
    def zeroOneVec(i: Int) = zeroVec upd (i, 1)
    // 0,0,0  1,0,0
    // 0,0,0  0,1,0
    // 0,0,0  0,0,1
    val coeffsSeq: Seq[Coeffs] = vecs map (solVal => Seq(Product(solVal), Product.ONE))
    val powersSeq: Seq[Powers] = (0 until dimension) map (i => Seq(zeroVec, zeroOneVec(i)))
    val tPolys: Polys = (coeffsSeq, powersSeq).zipped.toIndexedSeq map coeffRowsSeqToPoly
    tPolys map (_.skipZeroTerms)
  }

  private def coeffRowsSeqToPoly(coeffs: Coeffs, powers: Powers): Polynomial = {
    (coeffs zip powers).toIndexedSeq map Term.apply
  }
}
