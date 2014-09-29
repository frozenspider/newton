package org.newtonpolyhedron.utils.parsing

import org.newtonpolyhedron.entity.BigFrac

object ParseFormats {
  type Parse[A] = (String => A)

  val parseInt: Parse[BigInt] = BigInt(_: String)
  val parseFrac: Parse[BigFrac] = BigFracFormat.parse(_: String).asInstanceOf[BigFrac]
}
