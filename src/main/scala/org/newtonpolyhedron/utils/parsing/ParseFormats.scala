package org.newtonpolyhedron.utils.parsing

import org.newtonpolyhedron.NewtonImports._

object ParseFormats {
  type Parse[A] = (String => A)

  val parseInt: Parse[BigInt] =
    (s => BigInt(trimLeadingPlus(s)))
  val parseFrac: Parse[Rational] =
    (s => RationalFormat.parse(trimLeadingPlus(s)).asInstanceOf[Rational])
  def parseNum[N <: MPNumber](implicit mp: MathProcessor[N]): Parse[N] =
    parseFrac andThen mp.fromRational

  private def trimLeadingPlus(s: String): String = {
    val s2 = s.trim
    if (s2.startsWith("+")) s2.tail else s2
  }
}
