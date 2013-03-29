package org.newtonpolyhedron.entity

import scala.annotation.implicitNotFound
import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions
import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.Field

/** Wrapper around {@link org.apache.commons.math3.fraction.BigFraction} */
case class BigFrac(val underlying: BigFraction)
    extends ScalaNumber
    with ScalaNumericConversions
    with FieldElement[BigFrac]
    with Serializable {
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

  override def add(that: BigFrac) = this + that
  override def subtract(that: BigFrac) = this - that
  override def multiply(that: BigFrac) = this * that
  override def multiply(that: Int) = this * that
  override def divide(that: BigFrac) = this / that
  override def reciprocal = new BigFrac(underlying.reciprocal)
  override def negate = -this
  override def getField = BigFrac.BigFracField

  val isInt = underlying.getDenominator == 1
  override def isWhole = true
  override def doubleValue = this.underlying.doubleValue
  override def floatValue = this.underlying.floatValue
  override def longValue = this.underlying.longValue
  override def intValue = this.underlying.intValue

  override def equals(that: Any): Boolean = that match {
    case that: BigFrac    => this.underlying == that.underlying
    case that: BigInt     => this == BigFrac(that)
    case that: Int        => this == BigFrac(that)
    case that: Long       => this == BigFrac(that)
    case that: BigDecimal => that.toBigIntExact exists (x => this.underlying == BigFrac(x))
    case _                => false
  }
  override def hashCode = this.underlying.hashCode
  override def toString = this.underlying.toString
}

object BigFrac {
  val ZERO = new BigFrac(BigFraction.ZERO)
  val ONE = new BigFrac(BigFraction.ONE)
  val MINUS_ONE = new BigFrac(BigFraction.MINUS_ONE)

  def apply(n: Int) = new BigFrac(new BigFraction(n))
  def apply(n: BigInt) = new BigFrac(new BigFraction(n.underlying))
  def apply(n: Int, d: Int) = new BigFrac(BigFraction.getReducedFraction(n, d))
  def apply(n: BigInt, d: BigInt) = new BigFrac(new BigFraction(n.underlying, d.underlying).reduce())

  implicit def int2bigFrac(n: Int) = this(n)
  implicit def bigInt2bigFrac(n: BigInt) = this(n)

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

  implicit object BigFracField extends Field[BigFrac] {
    def getZero = BigFrac.ZERO
    def getOne = BigFrac.ONE
    def getRuntimeClass = classOf[BigFrac]
  }
}