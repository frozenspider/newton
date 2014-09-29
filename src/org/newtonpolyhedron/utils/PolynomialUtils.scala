package org.newtonpolyhedron.utils

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.equation.Equation
import org.newtonpolyhedron.utils.LanguageImplicits._

object PolynomialUtils {
  type Polynomial = IndexedSeq[Term]
  type Polys = IndexedSeq[Polynomial]
  type Equations = IndexedSeq[Equation]

  def zeroPoly(dim: Int): Polynomial =
    IndexedSeq(Term(Product.ZERO, IndexedSeq.fill[BigFrac](dim)(BigFrac.ZERO)))

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
        term.coeff.fracValue +
        ") * " +
        term.powers.mapWithIndex((pow, i) => s"x${i + 1}^($pow)").mkString(" * ")
      ) mkString ("", " + ", " = 0")
    }
  }

}