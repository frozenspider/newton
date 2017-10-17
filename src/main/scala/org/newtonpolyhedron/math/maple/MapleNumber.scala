package org.newtonpolyhedron.math.maple

import org.newtonpolyhedron.math.MPNumber

/**
 * @author FS
 */
class MapleNumber(_expr: String) extends MPNumber {
  def expr: String = _expr
  override def isValid: Boolean = true
  override def toString = "MapleNumber($expr)"
}

object MapleNumber {
  object Invalid extends MapleNumber("(invalid)") {
    override val isValid = false
    override def expr: String = fail()
    override def toString = "MapleNumber(Invalid)"

    private def fail() = throw new ArithmeticException("Can't operate on invalid value")
  }
}
