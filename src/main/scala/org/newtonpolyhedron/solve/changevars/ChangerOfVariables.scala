package org.newtonpolyhedron.solve.changevars

import org.newtonpolyhedron.NewtonImports._

import spire.compat._
import spire.implicits._

trait ChangerOfVariables[N <: MPNumber] {

  /**
   * Ordered by
   * 1) Ascending powers sum
   * 2) Descending powers
   */
  implicit val requiredTermOrdering: Ordering[Term[N]] = new Ordering[Term[N]] {
    override def compare(x: Term[N], y: Term[N]): Int = {
      val cmp1 = x.powers.sum compare y.powers.sum
      if (cmp1 != 0) cmp1
      else -(x.powers compare y.powers)
    }
  }

  /**
   * Executes the change-of-variables operations, substituting each variable in each term of original
   * polynomial with corresponding polynomial in substitution polynomials, thus expressing original variables
   * via their polynomial expansion.
   * <p>
   * Example:
   * <pre>
   * Original:
   * p = k1 * x^2 * y^2  +  k2 * x * z^3
   *
   * Substitution:
   * x = 2 * a^2 * b
   * y = a  +  b
   * z = a ^ 2 - 3 * b
   *
   * Change of Variables Result:
   * Expansion of
   * k1 * (2 * a^2 * b)^2 * (a  +  b)^2  +  k2 * (2 * a^2 * b) * (a ^ 2 - 3 * b)^3
   * <pre>
   * @param poly original polynomial
   * @param substs substitution polynomials
   * @return change of variables result
   */
  def changeVars(poly: Polynomial[N], substs: Polys[N]): Polynomial[N]

}
