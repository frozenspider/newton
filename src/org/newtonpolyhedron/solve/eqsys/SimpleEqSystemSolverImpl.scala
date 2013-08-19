package org.newtonpolyhedron.solve.eqsys

import scala.collection.IndexedSeq
import org.newtonpolyhedron.Polynomial
import org.newtonpolyhedron.Polys
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Product

/**
 * Is only capable of solving *simple* equation systems.
 * <p>
 * "Simple" here means that each equation should be a polynomial consisting of exactly two terms.
 */
class SimpleEqSystemSolverImpl extends EqSystemSolver {

  private type Replacement = Map[Int, Product]

  def whyCantSolve(system: Polys): Option[String] = {
    if (system.flatMap(poly => poly map (term => term.powers.dim)).distinct.size != 1)
      Some("Terms have different dimensions")
    else {
      val varsCounts = system.head.head.powers.dim
      if (system.size < varsCounts)
        Some("Not enough equations (${system.size}) for ${varsCounts} variables")
      else if (system exists (_.size != 2))
        Some("Each equation should consist of exactly two terms")
      else None
    }
  }

  def solve(system: Polys): Seq[FracMathVec] = {
    require(canSolve(system))
    val varsCounts = system.head.head.powers.dim
    val replacements = solveSimpleEqSysFor(system, (0 until varsCounts), Map.empty)
    val res = replacements.foldLeft(IndexedSeq.fill(varsCounts)(BigFrac.ZERO)) {
      case (acc, (idx, value)) => acc.updated(idx, value.fracValue)
    }
    Seq(res)
  }

  def solveSimpleEqSysFor(unsolved: Polys, termIndicesToReplace: Seq[Int], replacements: Replacement): Replacement = {
    if (termIndicesToReplace.isEmpty) {
      // We're done
      replacements
    } else {
      val replFor = termIndicesToReplace.head
      def replPowsAreEqual =
        (eq: Polynomial) => eq(0).powers(replFor) == eq(1).powers(replFor)
      // This will also include zero-powers (since 0 == 0)
      if (unsolved forall replPowsAreEqual) {
        // Variable isn't used, can just put anything. Zero is the easiest choice
        solveSimpleEqSysFor(unsolved, termIndicesToReplace.tail, replacements.updated(replFor, Product.ZERO))
      } else {
        val (chosenEq, restEqs) = {
          val foundIdx = unsolved.indexWhere(!replPowsAreEqual(_))
          assert(foundIdx >= 0)
          (unsolved(foundIdx), unsolved.zipWithIndex.filterNot(_._2 == foundIdx).unzip._1)
        }
        val (mainTerm, secTerm) = {
          val (t1, t2) = (chosenEq(0), chosenEq(1))
          val (p1, p2) = (t1.powers(replFor), t2.powers(replFor))
          val minPow = p1 min p2
          def adaptToMinPow(t: Term) =
            if (minPow < 0) t.mapPowers(pows => pows.updated(replFor, pows(replFor) - minPow)) else t
          val (t1n, t2n) = (adaptToMinPow(t1), adaptToMinPow(t2))
          if (p1 > p2)
            (t1n, t2n)
          else
            (t2n, t1n)
        }
        val powDiff = mainTerm.powers(replFor) - secTerm.powers(replFor)
        // Lets put secondary term to rhs and reduce
        val reducedM = mainTerm mapPowers (powers => powers.updated(replFor, 0))
        val reducedS = secTerm mapPowers (powers => powers.updated(replFor, 0))
        val currRepl = -(reducedS / reducedM) pow powDiff.inv
        val restReplaced = restEqs map representThrough(replFor, currRepl)
        val solutionForRest = solveSimpleEqSysFor(restReplaced, termIndicesToReplace.tail, replacements)
        val unrolledValue = currRepl.powers.elements.zipWithIndex.map {
          case (pow, idx) => if (pow == 0) Product.ONE else solutionForRest(idx) pow pow
        }
        val reduced = unrolledValue.reduce(_ * _) * currRepl.coeff
        require(reduced.isRational, s"Irratinal replacement for ${replFor + 1}'st term: ${reduced}")
        solutionForRest updated (replFor, reduced)
      }
    }
  }

  def representThrough(termIdx: Int, repr: Term)(eq: Polynomial): Polynomial = {
    require(repr.powers(termIdx) == 0)
    val res = for (term <- eq) yield {
      val srcPow = term.powers(termIdx)
      val poweredRepr = repr pow srcPow
      val powerlessTerm = term mapPowers (_.updated(termIdx, 0))
      val newTerm = poweredRepr * powerlessTerm
      newTerm
    }
    res
  }
}