package org.newtonpolyhedron.entity

import scala.annotation.implicitNotFound
import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions

import org.apache.commons.math3.fraction.BigFraction

/** Wrapper around {@link org.apache.commons.math3.fraction.BigFraction} */
class BigFrac(val underlying: BigFraction) extends ScalaNumber with ScalaNumericConversions {
  lazy val num = new BigInt(this.underlying.getNumerator)
  lazy val den = new BigInt(this.underlying.getDenominator)
  lazy val quotient = num / den
  lazy val remainder = num % den
  /** Behaves exactly as {@link Math#round(double)}.  */
  lazy val round = new BigInt((this.underlying add BigFraction.ONE_HALF).bigDecimalValue(java.math.BigDecimal.ROUND_FLOOR).toBigIntegerExact)

  def +(that: BigFrac) = new BigFrac(this.underlying add that.underlying)
  def +(that: BigInt) = new BigFrac(this.underlying add that.underlying)
  def +(that: Long) = new BigFrac(this.underlying add that)

  def -(that: BigFrac) = new BigFrac(this.underlying subtract that.underlying)
  def -(that: BigInt) = new BigFrac(this.underlying subtract that.underlying)
  def -(that: Long) = new BigFrac(this.underlying subtract that)

  def *(that: BigFrac) = new BigFrac(this.underlying multiply that.underlying)
  def *(that: BigInt) = new BigFrac(this.underlying multiply that.underlying)
  def *(that: Long) = new BigFrac(this.underlying multiply that)

  def /(that: BigFrac) = new BigFrac(this.underlying divide that.underlying)
  def /(that: BigInt) = new BigFrac(this.underlying divide that.underlying)
  def /(that: Long) = new BigFrac(this.underlying divide that)

  def unary_- = new BigFrac(this.underlying.negate)

  def compare(that: BigFrac): Int = this.underlying compareTo that.underlying
  def <=(that: BigFrac): Boolean = compare(that) <= 0
  def >=(that: BigFrac): Boolean = compare(that) >= 0
  def <(that: BigFrac): Boolean = compare(that) < 0
  def >(that: BigFrac): Boolean = compare(that) > 0

  def equals(that: BigFrac): Boolean = this.underlying equals that.underlying

  val isInt = underlying.getDenominator == 1
  def isWhole = true
  def doubleValue = this.underlying.doubleValue
  def floatValue = this.underlying.floatValue
  def longValue = this.underlying.longValue
  def intValue = this.underlying.intValue
}

object BigFrac {
  def apply(n: Int, d: Int) = new BigFrac(BigFraction.getReducedFraction(n, d))
  def apply(n: BigInt, d: BigInt) = new BigFrac(new BigFraction(n.underlying, d.underlying).reduce())
  implicit object BigFracNumeric extends Numeric[BigFrac] {
    override def compare(a: BigFrac, b: BigFrac) = a compare b
    override def plus(x: BigFrac, y: BigFrac): BigFrac = x + y
    override def minus(x: BigFrac, y: BigFrac): BigFrac = x - y
    override def times(x: BigFrac, y: BigFrac): BigFrac = x * y
    override def negate(x: BigFrac): BigFrac = -x
    override def fromInt(x: Int): BigFrac = BigFrac(x, 1)
    override def toInt(x: BigFrac): Int = x.underlying.intValue
    override def toLong(x: BigFrac): Long = x.underlying.longValue
    override def toFloat(x: BigFrac): Float = x.underlying.floatValue
    override def toDouble(x: BigFrac): Double = x.underlying.doubleValue
  }
}