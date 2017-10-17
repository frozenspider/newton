package org.newtonpolyhedron.entity.equation

import org.newtonpolyhedron.utils.PolynomialUtils._
import org.newtonpolyhedron.math.MathImports._

case class Equation[N <: MPNumber](lhs: Polynomial[N], sign: EquationSign, rhs: Polynomial[N])

object Equation {
  def makeEqualToZero[N <: MPNumber](lhs: Polynomial[N])(implicit mp: MathProcessor[N]): Equation[N] = {
    Equation(lhs, EquationSign.Equals, zeroPoly(lhs.headOption map (_.powers.size) getOrElse 0))
  }
}
