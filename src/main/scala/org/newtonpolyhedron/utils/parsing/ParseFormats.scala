package org.newtonpolyhedron.utils.parsing

import spire.math.Rational

object ParseFormats {
  type Parse[A] = (String => A)

  val parseInt: Parse[BigInt] =
    (s => BigInt(trimLeadingPlus(s)))
  val parseFrac: Parse[Rational] =
    (s => RationalFormat.parse(trimLeadingPlus(s)).asInstanceOf[Rational])

  private def trimLeadingPlus(s: String): String = {
    val s2 = s.trim
    if (s2.startsWith("+")) s2.tail else s2
  }
}
