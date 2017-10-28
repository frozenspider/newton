package org.newtonpolyhedron.solve.power

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.solve.eqsys.EqSystemSolver
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker
import spire.math.Rational

class PowerTransformationSolverImpl[N <: MPNumber](
  val umm:         UnimodularMatrixMaker[N],
  val eqSysSolver: EqSystemSolver[N]
)(implicit mp: MathProcessor[N])
    extends PowerTransformationSolver[N] {

  private type Powers = Seq[NumVec[N]]
  private type Coeffs = Seq[N]

  private def vecn(s: Seq[N]) = NumVec(s: _*)
  private def vecf(s: Seq[Rational]) = NumVec(s.map(mp.fromRational): _*)

  override def generateAlphaFromTerms(powersSeqs: Seq[Seq[NumVec[N]]]): Matrix[N] = {
    // TODO: Make sure this works for more than 2 polys
    val dimension = powersSeqs.size + 1
    def lastRowMinors(m: Matrix[N]): Seq[N] = {
      (0 until m.colCount) map { c => m.minor(m.rowCount - 1, c) }
    }
    require(
      powersSeqs forall (_ forall (_.length == dimension)),
      "Each term should have the same dimension: number of pairs + 1"
    )
    val pairsStream: Stream[Seq[(NumVec[N], NumVec[N])]] = choosePairs(powersSeqs)
    val matrices = pairsStream map { pairs =>
      val matrixBase = (pairs map {
        case (powers1, powers2) => powers1 - powers2
      }) :+ NumVec.zero(dimension)
      Matrix(matrixBase)
    }
    if (matrices.isEmpty)
      throw new IllegalArgumentException("Pairs are not provided")
    val nonZeroMatrices = matrices filter { matrix =>
      !lastRowMinors(matrix).contains(0)
    }
    if (nonZeroMatrices.isEmpty)
      throw new IllegalArgumentException("All pre-alpha matrices have zero minors")
    val alphasStream: Stream[Matrix[N]] =
      nonZeroMatrices map { matrix =>
        val alpha = umm unimodularFrom matrix
        assert(alpha.elementsByRow map (_._3) forall (_.isIntegral))
        assert(alpha.det == mp.one)
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

  private def matrixByRows(m: Matrix[N]): Seq[NumVec[N]] =
    (m.elementsByRow map (_._3) grouped (m.colCount) map vecn).toIndexedSeq

  private def inverse(m: Matrix[N]): Matrix[N] = {
    val inv = m.inverse
    assert(inv.elementsByRow map (_._3) forall (_.isIntegral))
    inv
  }

  private def getLowestPowers(powers: Powers) =
    vecn(powers reduce ((_, _).zipped map (spire.math.min[N])))

  override def substitute(poly: Polynomial[N], alpha: Matrix[N]): Polynomial[N] = {
    require(alpha.isSquare, "Alpha matrix should be square")
    val alphaInvByRows = matrixByRows(inverse(alpha))
    val rawPows: IndexedSeq[NumVec[N]] =
      poly.powers map (p => (alphaInvByRows, p).zipped map ((alphaRow, polyPow) => alphaRow * polyPow) reduce (_ + _))
    val lowestPows = getLowestPowers(rawPows)
    val resPows = rawPows map (_ - lowestPows)
    //    println(" === Alpha Inv:       " + alphaInvByRows)
    //    println(" === Unreduced:       " + rawPows)
    //    println(" === Lowest powers:   " + lowestPows)
    poly.coeffs zip resPows map (p => Term(p))
  }

  //
  // =========================
  //

  override def solveShortSubstitutesSystem(simpleSys: Polys[N]): NumVec[N] = {
    require(simpleSys forall (_ forall (t => t.powers.last == mp.zero)))
    // Remove last (zero) component
    val truncatedSimpleSys: Polys[N] = simpleSys map (_ map (_.mapPowers(_ dropRight 1)))
    val sol: Powers = eqSysSolver.solve(truncatedSimpleSys)
    assert(sol.tail.isEmpty)
    // Add it back
    sol.head :+ mp.zero
  }

  //
  // =========================
  //

  override def varChangeFromShortSubsSolution(vecs: NumVec[N]): Polys[N] = {
    val dimension = vecs.size
    val zeroVec = NumVec.zero[N](dimension)
    def zeroOneVec(i: Int) = zeroVec upd (i, mp.one)
    // 0,0,0  1,0,0
    // 0,0,0  0,1,0
    // 0,0,0  0,0,1
    val coeffsSeq: Seq[Coeffs] = vecs map (solVal => Seq(solVal, mp.one))
    val powersSeq: Seq[Powers] = (0 until dimension) map (i => Seq(zeroVec, zeroOneVec(i)))
    val tPolys: Polys[N] = (coeffsSeq, powersSeq).zipped.toIndexedSeq map coeffRowsSeqToPoly
    tPolys map (_.skipZeroTerms)
  }

  private def coeffRowsSeqToPoly(coeffs: Coeffs, powers: Powers): Polynomial[N] = {
    (coeffs zip powers).toIndexedSeq map (p => Term(p))
  }
}
