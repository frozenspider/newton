package org.newtonpolyhedron.utils

import org.fs.utility.Imports._
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.equation.Equation
import org.newtonpolyhedron.math.MathImports._

import spire.compat._

// TODO: Move, rename to PolynomialImports
trait PolynomialUtils {
  /** Polynominal - a sum of terms */
  type Polynomial[N <: MPNumber] = IndexedSeq[Term[N]]
  /** List of polynominals */
  type Polys[N <: MPNumber] = IndexedSeq[Polynomial[N]]
  /** List of equations */
  type Equations[N <: MPNumber] = IndexedSeq[Equation[N]]

  def zeroPoly[N <: MPNumber](dim: Int)(implicit mp: MathProcessor[N]): Polynomial[N] =
    IndexedSeq(Term(mp.zero, IndexedSeq.fill[N](dim)(mp.zero)))

  /**
   * Contains some basic polynomial-related operations - multiplication, raising to powers, etc.
   */
  implicit class RichPolynomial[N <: MPNumber](poly: Polynomial[N])(implicit mp: MathProcessor[N]) {

    def **(pow: Int): Polynomial[N] = {
      require(pow >= 0, "Can't raise polynomial to negative power")
      if (pow == 0) IndexedSeq(Term.one(poly.headOption map (_.powers.size) getOrElse 0))
      else if (pow == 1) poly
      else {
        // TODO: Use Newton Binomial
        Seq.fill(pow)(poly) reduce (_ * _)
      }
    }

    def *(that: Polynomial[N]): Polynomial[N] = {
      val preRes = for {
        t1 <- poly
        t2 <- that
      } yield t1 * t2
      preRes.collapseDups
    }

    /** Collapses duplicates and removes zeros */
    def collapseDups: Polynomial[N] = {
      val res = poly groupBy (_.powers) map (_._2) map (_.reduceLeft((t1, t2) => t1 withCoeff (t1.coeff + t2.coeff)))
      res.toIndexedSeq.skipZeroTerms
    }

    /** Skip zeros - terms with zero coefficient */
    def skipZeroTerms: Polynomial[N] =
      poly filterNot (x => mp.isZero(x.coeff))

    def powers =
      poly map (_.powers)

    def coeffs =
      poly map (_.coeff)

    def toPlainString: String = {
      poly map (term => "(" +
        term.coeff.toString + // TODO: term.coeff.toRational
        ") * " +
        term.powers.mapWithIndex((pow, i) => s"x${i + 1}^($pow)").mkString(" * ")) mkString ("", " + ", " = 0")
    }
  }

  implicit class SubstitutionPolynomial[N <: MPNumber](p: Polynomial[N])(implicit mp: MathProcessor[N]) {
    /** Substitute variables in all terms with the given values */
    def withValues(vs: Seq[N]): Seq[N] = {
      val substitutedTerms = p map { term =>
        require(vs.size == term.powers.length, s"Dimension of term $term doesn't equal ${vs.size}")
        val powered = vs zip term.powers map {
          case (value, power) => value ** power
        }
        term.coeff * powered.product
      }
      substitutedTerms
    }

    /** Substitute variables in all terms with the given values and sum the result */
    def totalWithValues(vs: Seq[N]): N = {
      withValues(vs).sum
    }

    /** Substitute variables in all terms with the given values and sum the result as far as possible */
    def totalWithValuesNonStrict(vs: Seq[N]): Seq[N] = {
      reduceSum(withValues(vs))
    }

    /** Substitute variables in all terms with the given values and checks the resulting sum for being zero */
    def isZeroWithValues(vs: Seq[N]): Boolean = {
      val substituted = withValues(vs)
      val reduced = reduceSum(substituted)
      reduced.tail.isEmpty && reduced.head.isZero
    }
  }

  private def isSummable[N <: MPNumber](p1: N, p2: N)(implicit mp: MathProcessor[N]): Boolean =
    try {
      p1 + p2
      true
    } catch {
      case ex: IllegalArgumentException => false
    }

  private def reduceSum[N <: MPNumber](sum: Seq[N])(implicit mp: MathProcessor[N]): Seq[N] = {
    def reduceSumInner(accUnreducible: Seq[N], remainings: Seq[N]): Seq[N] =
      remainings match {
        case e if e.isEmpty =>
          accUnreducible
        case x +: xs =>
          val xsPairs = xs mapWithIndex ((xx, i) => (xx, xs.patch(i, Nil, 1)))
          val stepOption = xsPairs.collectFirst {
            case (potentialPair, restWithoutSelected) if isSummable(x, potentialPair) =>
              (x + potentialPair, restWithoutSelected)
          }
          stepOption match {
            case Some((sum, rest)) => reduceSumInner(accUnreducible, sum +: rest)
            case None              => reduceSumInner(accUnreducible :+ x, xs)
          }
      }
    reduceSumInner(Nil, sum).toIndexedSeq
  }

}

object PolynomialUtils extends PolynomialUtils
