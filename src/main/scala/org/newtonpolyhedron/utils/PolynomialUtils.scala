package org.newtonpolyhedron.utils

import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.equation.Equation
import org.newtonpolyhedron.utils.LanguageImplicits._

import org.fs.utility.Imports._

import spire.math.Rational


object PolynomialUtils {
  type Polynomial = IndexedSeq[Term]
  type Polys = IndexedSeq[Polynomial]
  type Equations = IndexedSeq[Equation]

  def zeroPoly(dim: Int): Polynomial =
    IndexedSeq(Term(Product.zero, IndexedSeq.fill[Rational](dim)(Rational.zero)))

  /**
   * Contains some basic polynomial-related operations - multiplication, raising to powers, etc.
   */
  implicit class RichPolynomial(poly: Polynomial) {

    def pow(pow: Int): Polynomial = {
      require(pow >= 0, "Can't raise polynomial to negative power")
      if (pow == 0) IndexedSeq(Term.one(poly.headOption map (_.powers.size) getOrElse 0))
      else if (pow == 1) poly
      else {
        // TODO: Use Newton Binomial
        Seq.fill(pow)(poly) reduce (_ * _)
      }
    }

    def *(that: Polynomial): Polynomial = {
      val preRes = for {
        t1 <- poly
        t2 <- that
      } yield t1 * t2
      preRes.collapseDups
    }

    /** Collapses duplicates and removes zeros */
    def collapseDups: Polynomial = {
      val res = poly groupBy (_.powers) map (group => group._2) map (_.reduceLeft((t1, t2) => t1 withCoeff (t1.coeff + t2.coeff)))
      res.toIndexedSeq.skipZeroTerms
    }

    /** Skip zeros - terms with zero coefficient */
    def skipZeroTerms: Polynomial =
      poly filterNot (_.coeff.isZero)

    def powers =
      poly map (_.powers)

    def coeffs =
      poly map (_.coeff)

    def toPlainString: String = {
      poly map (term => "(" +
        term.coeff.toRational +
        ") * " +
        term.powers.mapWithIndex((pow, i) => s"x${i + 1}^($pow)").mkString(" * ")
      ) mkString ("", " + ", " = 0")
    }
  }

  implicit class SubstitutionPolynomial(p: Polynomial) {
    /** Substitute variables in all terms with the given values */
    def withValues(vs: Seq[Product]): Seq[Product] = {
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
    def totalWithValues(vs: Seq[Product]): Product = {
      withValues(vs).sum
    }

    /** Substitute variables in all terms with the given values and sum the result as far as possible */
    def totalWithValuesNonStrict(vs: Seq[Product]): Seq[Product] = {
      reduceSum(withValues(vs))
    }

    /** Substitute variables in all terms with the given values and checks the resulting sum for being zero */
    def isZeroWithValues(vs: Seq[Product]): Boolean = {
      val substituted = withValues(vs)
      val reduced = reduceSum(substituted)
      reduced.tail.isEmpty && reduced.head.isZero
    }
  }

  private def isSummable(p1: Product, p2: Product): Boolean =
    try {
      p1 + p2
      true
    } catch {
      case ex: IllegalArgumentException => false
    }

  private def reduceSum(sum: Seq[Product]): Seq[Product] = {
    def reduceSumInner(accUnreducible: Seq[Product], remainings: Seq[Product]): Seq[Product] =
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
