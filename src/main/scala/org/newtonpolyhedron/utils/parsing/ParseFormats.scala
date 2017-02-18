package org.newtonpolyhedron.utils.parsing

import org.newtonpolyhedron.entity.BigFrac

object ParseFormats {
  type Parse[A] = (String => A)

  val parseInt: Parse[BigInt] =
    (s => BigInt(trimLeadingPlus(s)))
  val parseFrac: Parse[BigFrac] =
    (s => BigFracFormat.parse(trimLeadingPlus(s)).asInstanceOf[BigFrac])

  private def trimLeadingPlus(s: String): String = {
    val s2 = s.trim
    if (s2.startsWith("+")) s2.tail else s2
  }
}
