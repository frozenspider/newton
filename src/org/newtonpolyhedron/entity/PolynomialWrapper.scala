package org.newtonpolyhedron.entity

object PolynomialWrapper {
  type Polynomial = IndexedSeq[Term]

  /**
   * Contains some basic polynomial-related operations - multiplication, raising to powers, etc.
   */
  implicit class RichPolynomial(poly: IndexedSeq[Term]) {

    def pow(pow: Int): Polynomial = {
      require(pow >= 0, "Can't reaise polynomial to negative power")
      if (pow == 0) IndexedSeq(Term.zero(poly.headOption map (_.powers.dim) getOrElse 0))
      else if (pow == 1) poly
      else {
        // TODO: Use Newton Binomial
        Seq.fill(pow)(poly) reduce (_ * _)
      }
    }

    def *(that: Polynomial): Polynomial = {
      val preRes = for {
        Term(c1, p1) <- poly
        Term(c2, p2) <- that
      } yield Term(c1 * c2, p1 + p2)
      preRes.collapseDups
    }

    def collapseDups: Polynomial = {
      val res = poly groupBy (_.powers) map (group => group._2) map (_.reduceLeft((t1, t2) => t1 withCoeff (t1.coeff + t2.coeff)))
      res.toIndexedSeq.skipZeroTerms
    }

    def skipZeroTerms: Polynomial =
      poly filterNot (_.coeff.isZero)

    def powers =
      poly map (_.powers)

    def coeffs =
      poly map (_.coeff)
  }
}