package org.newtonpolyhedron.entity.equation

/** Relational sign between sides of an (in)equation: `=`, `<`, `>=`, etc. */
sealed trait RelationalSign

object RelationalSign {
  case object Equals extends RelationalSign { override val toString = "=" }
  case object Greater extends RelationalSign { override val toString = ">" }
  case object GreaterEq extends RelationalSign { override val toString = ">=" }
  case object Less extends RelationalSign { override val toString = "<" }
  case object LessEq extends RelationalSign { override val toString = "<=" }
}
