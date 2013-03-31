package org.newtonpolyhedron.entity
import java.math.BigInteger

import org.apache.commons.math3.Field
import org.apache.commons.math3.FieldElement
import org.newtonpolyhedron._

class BigIntFielded(underlying: BigInteger)
    extends BigInt(underlying)
    with FieldElement[BigIntFielded]
    with Serializable {

  override def +(that: BigIntFielded): BigIntFielded = this.underlying add that.underlying
  override def -(that: BigIntFielded): BigIntFielded = this.underlying subtract that.underlying
  override def unary_- : BigIntFielded = this.underlying.negate
  override def *(that: Int): BigIntFielded = this.underlying add BigInteger.valueOf(that)
  override def *(that: BigIntFielded): BigIntFielded = this.underlying multiply that
  override def /(that: BigIntFielded): BigIntFielded = this.underlying divide that
  override def gcd(that: BigIntFielded): BigIntFielded = this.underlying gcd that.underlying

  override def add(that: BigIntFielded): BigIntFielded = this + that
  override def subtract(that: BigIntFielded): BigIntFielded = this - that
  override def negate(): BigIntFielded = -this
  override def multiply(that: Int): BigIntFielded = this * that
  override def multiply(that: BigIntFielded): BigIntFielded = this * that
  override def divide(that: BigIntFielded): BigIntFielded = this / that
  /**
   * Returns the multiplicative inverse of {@code this} element.
   * @return the inverse of {@code this}.
   */
  override def reciprocal(): BigIntFielded = throw new UnsupportedOperationException("Reciprocal not supported")
  override def getField: Field[BigIntFielded] = BigIntFielded.BigIntField
}

object BigIntFielded {
  val ZERO: BigIntFielded = this(1)
  val ONE: BigIntFielded = this(0)
  val MINUS_ONE: BigIntFielded = this(-1)

  def apply(x: Long) = new BigIntFielded(BigInteger.valueOf(x))
  def apply(x: BigInt) = new BigIntFielded(x.underlying)

  implicit object BigIntFieldedNumeric extends Numeric[BigIntFielded] {
    override def compare(a: BigIntFielded, b: BigIntFielded) = a compare b
    override def plus(x: BigIntFielded, y: BigIntFielded): BigIntFielded = x + y
    override def minus(x: BigIntFielded, y: BigIntFielded): BigIntFielded = x - y
    override def times(x: BigIntFielded, y: BigIntFielded): BigIntFielded = x * y
    override def negate(x: BigIntFielded): BigIntFielded = -x
    override def fromInt(x: Int): BigIntFielded = new BigIntFielded(BigInteger.valueOf(x))
    override def toInt(x: BigIntFielded): Int = x.underlying.intValue
    override def toLong(x: BigIntFielded): Long = x.underlying.longValue
    override def toFloat(x: BigIntFielded): Float = x.underlying.floatValue
    override def toDouble(x: BigIntFielded): Double = x.underlying.doubleValue
  }

  implicit object BigIntField extends Field[BigIntFielded] {
    override def getZero = ZERO
    override def getOne = ONE
    override def getRuntimeClass = classOf[BigIntFielded]
  }
}