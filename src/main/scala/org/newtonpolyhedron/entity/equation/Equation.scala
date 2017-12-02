package org.newtonpolyhedron.entity.equation

import org.newtonpolyhedron.utils.PolynomialUtils._
import org.newtonpolyhedron.math.MathImports._

/**
 * Polynomial (in)equation, consisting of left-hand side, right-hand side and a relational sign between them.
 *
 * @author FS
 */
case class Equation[N <: MPNumber](lhs: Polynomial[N], sign: RelationalSign, rhs: Polynomial[N])

object Equation {
  def makeEqualToZero[N <: MPNumber](lhs: Polynomial[N])(implicit mp: MathProcessor[N]): Equation[N] = {
    Equation(lhs, RelationalSign.Equals, zeroPoly(lhs.headOption map (_.powers.size) getOrElse 0))
  }
}
