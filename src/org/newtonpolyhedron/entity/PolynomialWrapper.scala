package org.newtonpolyhedron.entity

import org.newtonpolyhedron.Polynomial

object PolynomialWrapper {

  /**
   * Contains some basic polynomial-related operations - multiplication, raising to powers, etc.
   */
  implicit class RichPolynomial(poly: IndexedSeq[Term]) {

    def pow(pow: Int): Polynomial = {
      require(pow >= 0, "Can't raise polynomial to negative power")
      if (pow == 0) IndexedSeq(Term.one(poly.headOption map (_.powers.dim) getOrElse 0))
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
  }
}