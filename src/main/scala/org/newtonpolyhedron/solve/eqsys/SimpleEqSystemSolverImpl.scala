package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.NewtonImports._

import spire.compat._
import spire.math.Rational

/**
 * Solver, only capable of solving *simple* systems of equations.
 * <p>
 * "Simple" here means that each equation should be a polynomial consisting of exactly two terms.
 */
class SimpleEqSystemSolverImpl[N <: MPNumber](implicit mp: MathProcessor[N]) extends EqSystemSolver[N] {

  private type Replacement = Map[Int, N]

  def whyCantSolve(system: Polys[N]): Option[String] = {
    val dimensions = for (poly <- system; term <- poly) yield term.powers.size
    if (dimensions.distinct.size != 1)
      Some("Terms have different dimensions")
    else {
      val varsCounts = system.head.head.powers.size
      if (system.size < varsCounts)
        Some("Not enough equations (${system.size}) for ${varsCounts} variables")
      else if (system exists (_.size != 2))
        Some("Each equation should consist of exactly two terms")
      else None
    }
  }

  def solve(system: Polys[N]): Seq[NumVec[N]] = {
    require(canSolve(system))
    val varsCounts = system.head.head.powers.size
    val replacements = solveSimpleEqSysFor(system, (0 until varsCounts), Map.empty)
    val res = replacements.foldLeft(IndexedSeq.fill(varsCounts)(mp.zero)) {
      case (acc, (idx, value)) => acc.updated(idx, value)
    }
    Seq(res)
  }

  def solveSimpleEqSysFor(unsolved: Polys[N], termIndicesToReplace: Seq[Int], replacements: Replacement): Replacement = {
    if (termIndicesToReplace.isEmpty) {
      // We're done
      replacements
    } else {
      val replFor = termIndicesToReplace.head
      def replPowsAreEqual(eq: Polynomial[N]) =
        eq(0).powers(replFor) == eq(1).powers(replFor)
      def replPowsAreUnequal(eq: Polynomial[N]) =
        !replPowsAreEqual(eq)
      def excludeElementByIdx(xs: Polys[N], i: Int) =
        for ((x, idx) <- xs.zipWithIndex if idx != i) yield x
      // This will also include zero-powers (since 0 == 0)
      if (unsolved forall replPowsAreEqual) {
        // Variable isn't used, can just put anything. Zero is the easiest choice
        solveSimpleEqSysFor(unsolved, termIndicesToReplace.tail, replacements.updated(replFor, mp.zero))
      } else {
        val (chosenEq, restEqs) = {
          val foundIdx = unsolved indexWhere replPowsAreUnequal
          assert(foundIdx >= 0)
          (unsolved(foundIdx), excludeElementByIdx(unsolved, foundIdx))
        }
        val (mainTerm, secTerm) = {
          val (t1, t2) = (chosenEq(0), chosenEq(1))
          val (p1, p2) = (t1.powers(replFor), t2.powers(replFor))
          val minPow = spire.math.min(p1, p2)
          def adaptToMinPow(t: Term[N]): Term[N] =
            if (minPow < mp.zero) t.mapPowers(pows => pows.updated(replFor, pows(replFor) - minPow)) else t
          val (t1n, t2n) = (adaptToMinPow(t1), adaptToMinPow(t2))
          if (p1 > p2)
            (t1n, t2n)
          else
            (t2n, t1n)
        }
        val powDiff = mainTerm.powers(replFor) - secTerm.powers(replFor)
        // Lets put secondary term to rhs and reduce
        val reducedM = mainTerm mapPowers (powers => powers.upd(replFor, mp.zero))
        val reducedS = secTerm mapPowers (powers => powers.upd(replFor, mp.zero))
        val currRepl = -(reducedS / reducedM) ** powDiff.inverse
        val restReplaced = restEqs map representThrough(replFor, currRepl)
        val solutionForRest = solveSimpleEqSysFor(restReplaced, termIndicesToReplace.tail, replacements)
        val unrolledValue = currRepl.powers.mapWithIndex { (pow, idx) =>
          if (pow.isZero) mp.one else (solutionForRest(idx) ** pow)
        }
        val reduced = unrolledValue.reduce(_ * _) * currRepl.coeff
        require(reduced.isRational, s"Irratinal replacement for ${replFor + 1}'st term: ${reduced}")
        solutionForRest updated (replFor, reduced)
      }
    }
  }

  def representThrough(termIdx: Int, repr: Term[N])(eq: Polynomial[N]): Polynomial[N] = {
    require(repr.powers(termIdx) == mp.zero)
    val res = for (term <- eq) yield {
      val srcPow = term.powers(termIdx)
      val poweredRepr = repr ** srcPow
      val powerlessTerm = term mapPowers (_.upd(termIdx, mp.zero))
      val newTerm = poweredRepr * powerlessTerm
      newTerm
    }
    res
  }
}
