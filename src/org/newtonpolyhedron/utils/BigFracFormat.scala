package org.newtonpolyhedron.utils

import java.text.ParsePosition

import scala.math.BigDecimal

import org.apache.commons.math3.fraction.AbstractFormat
import org.apache.commons.math3.fraction.BigFractionFormat
import org.newtonpolyhedron.entity.BigFrac

class BigFracFormat extends AbstractFormat {

  private val bigFrationFormat = new BigFractionFormat

  override def parse(source: String, pos: ParsePosition): BigFrac = {
    // Try to parse fraction as usual
    val defaultResult = bigFrationFormat.parse(source, pos)
    if (defaultResult != null)
      BigFrac(defaultResult)
    else {
      // Usual parsing failed (errorIndex has been set)
      val initialIndex = pos.getIndex

      // Parse whitespace
      AbstractFormat.parseAndIgnoreWhitespace(source, pos)

      // Parse decimal
      parseNextBigDecimal(source, pos) match {
        case None => {
          pos.setIndex(initialIndex)
          null
        }
        case Some(dec) => {
          // Unset error index
          pos.setErrorIndex(-1)
          val unscaled = BigInt(dec.underlying.unscaledValue)
          val scale = dec.scale
          val ten = BigInt(10)
          if (scale >= 0)
            BigFrac(unscaled, ten pow scale)
          else
            BigFrac(unscaled * (ten pow (-scale)))
        }
      }

    }
  }

  private def parseNextBigDecimal(source: String, pos: ParsePosition): Option[BigDecimal] = {
    val start = pos.getIndex
    var end = if (source.charAt(start) == '-') start + 1 else start
    while (end < source.length() && (Character.isDigit(source.charAt(end)) || source.charAt(end) == '.')) {
      end += 1
    }

    try {
      val n = BigDecimal(source.substring(start, end))
      pos.setIndex(end)
      Some(n)
    } catch {
      // Do not change the error index, it's already set by super.parse() call
      case nfe: NumberFormatException => None
    }
  }
}