package org.newtonpolyhedron.math.maple

import org.newtonpolyhedron.math.MathImports._

/**
 * @author FS
 */
class SageNumber(_expr: String) extends MPNumber {
  def expr: String = _expr
  override def isValid: Boolean = true
  override def toString = "SageNumber($expr)"
}

object SageNumber {
  object Invalid extends SageNumber("(invalid)") {
    override val isValid = false
    override def expr: String = fail()
    override def toString = "SageNumber(Invalid)"

    private def fail() = throw new ArithmeticException("Can't operate on invalid value")
  }
}
