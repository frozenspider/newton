package org.newtonpolyhedron.utils

object StringUtils {

  private def maxLength(s: Seq[Seq[_]]) = s.foldLeft(0)(_ max _.size)
  private def maxStrLength(s: Seq[String]) = s.foldLeft(0)(_ max _.size)

  def appendToRight(spaceWidth: Int, strs: CharSequence*): String = {
    val linesList = strs map (_.toString.split("[\\r\\n]+").toIndexedSeq)

    // Make lists same size
    val maxSize = maxLength(linesList)
    if (maxSize == 0) ""
    else {
      val extLines = linesList map (lines => lines ++ Seq.fill(maxSize - lines.size)(""))
      val maxLengths = extLines map maxStrLength

      // Right-pad all lines with spaces (including separator)
      val rightPadded = (extLines zip maxLengths) map {
        case (lines, maxLen) => lines map (org.apache.commons.lang3.StringUtils.rightPad(_, maxLen + spaceWidth))
      }

      val sb = new StringBuilder
      for (i <- 0 until maxSize) {
        rightPadded map (sb ++= _(i))
        sb += '\n'
      }
      sb.toString.trim
    }
  }
}