package org.newtonpolyhedron.entity.equation

import org.newtonpolyhedron.utils.PolynomialUtils._

case class Equation(lhs: Polynomial, sign: EquationSign, rhs: Polynomial)

object Equation {
  def makeEqualToZero(lhs: Polynomial): Equation = {
    Equation(lhs, EquationSign.Equals, zeroPoly(lhs.headOption map (_.powers.size) getOrElse 0))
  }
}
