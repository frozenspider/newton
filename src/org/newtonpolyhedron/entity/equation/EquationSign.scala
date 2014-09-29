package org.newtonpolyhedron.entity.equation

sealed trait EquationSign

object EquationSign {
  case object Equals extends EquationSign { override val toString = "=" }
  case object Greater extends EquationSign { override val toString = ">" }
  case object GreaterEq extends EquationSign { override val toString = ">=" }
  case object Less extends EquationSign { override val toString = "<" }
  case object LessEq extends EquationSign { override val toString = "<=" }
}
