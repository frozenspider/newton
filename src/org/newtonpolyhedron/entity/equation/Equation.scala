package org.newtonpolyhedron.entity.equation

import org.newtonpolyhedron._

case class Equation(lhs: Polynomial, sign: EquationSign, rhs: Polynomial)

object Equation {
  def makeEqualToZero(lhs: Polynomial): Equation = {
    Equation(lhs, EquationSign.Equals, zeroPoly(lhs.headOption map (_.powers.dim) getOrElse 0))
  }
}