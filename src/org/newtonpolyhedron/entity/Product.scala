package org.newtonpolyhedron.entity

import scala.math.ScalaNumber
import org.newtonpolyhedron.utils.MathUtils
import scala.collection.immutable.SortedSet
import scala.math.ScalaNumericConversions

/**
 * Represents a number as a product of powers of its prime factors.
 * <p>
 * E.g. 17 will be represented as 17^1, while 12 would be 2^2 * 3^1.
 * <p>
 * Only supports multiplication and power operations.
 */
case class Product(val signum: Int, val underlying: Map[Int, BigFrac])
    extends ScalaNumber
    with ScalaNumericConversions
    with Ordered[Product] {
  require((-1 to 1) contains signum, "Sign should be -1, 0 or 1")
  require(if (signum == 0) underlying.isEmpty else true, "For zero product, multipliers should be empty")

  def isZero = signum == 0

  def *(that: Product): Product =
    if (this.signum == 0 || that.signum == 0) Product.ZERO
    else new Product(this.signum * that.signum, mergePowers(this.underlying, that.underlying))
  def *(that: BigInt): Product = this * Product(that)
  def *(that: BigFrac): Product = this * Product(that)
  def *(that: Int): Product = this * Product(that)

  def /(that: Product): Product = {
    require(that.signum != 0, "Divide by zero")
    if (this.signum == 0) Product.ZERO
    else new Product(this.signum * that.signum, mergePowers(this.underlying, that.underlying map (p => (p._1, -p._2))))
  }
  def /(that: BigInt): Product = this / Product(that)
  def /(that: BigFrac): Product = this / Product(that)
  def /(that: Int): Product = this / Product(that)

  def +(that: Product): Product = {
    val (commonFactors, thisFactors, thatFactors) = extractCommonFactors(this.underlying, that.underlying)
    val common = new Product(1, commonFactors)
    val part1 = new Product(this.signum, thisFactors)
    val part2 = new Product(that.signum, thatFactors)
    val partSum = Product(part1.fracValue + part2.fracValue)
    val res = common * partSum
    res
  }
  def +(that: Int): Product = this + Product(that)
  def +(that: BigFrac): Product = this + Product(that)
  def -(that: Product): Product = this + -that
  def -(that: Int): Product = this + Product(-that)
  def -(that: BigFrac): Product = this + Product(-that)
  def unary_- : Product = new Product(-signum, underlying)

  def sqrt = this pow BigFrac(1, 2)
  def pow(p: BigFrac): Product = {
    if (this.signum == 0 && p == 0) Product.ONE
    else new Product(if (p == 0 || p.num % 2 == 0) 1 else this.signum, this.underlying map (e => (e._1, e._2 * p)) filter (_._2 != 0))
  }
  def pow(p: Int): Product = this pow BigFrac(p)

  /** Very ineffective! */
  override def compare(that: Product): Int = this.fracValue compare that.fracValue

  /** Whether or not this product can be represented as a precise fraction value */
  lazy val isRational = underlying forall (_._2.den == 1)
  override def isWhole = true
  override def longValue = fracValue.toLong
  override def intValue = fracValue.toInt
  lazy val fracValue =
    if (signum == 0) BigFrac.ZERO else {
      if (!isRational) throw new ArithmeticException("Not a valid fraction")
      val folded = (underlying foldLeft BigFrac.ONE) {
        case (acc, (v, p)) => acc * (BigFrac(v) pow p.num.toInt)
      }
      signum * folded
    }
  override def floatValue = doubleValue.toFloat
  override lazy val doubleValue =
    if (signum == 0) 0.0d else {
      val folded = (underlying foldLeft 1.0d) {
        case (acc, (v, p)) => acc * math.pow(v, p.doubleValue)
      }
      signum * folded
    }

  override def toString = {
    if (isRational) fracValue.toString
    else toStructuredString
  }

  def toStructuredString = {
    val str = if (this.underlying.isEmpty) {
      signum match {
        case 0  => "0"
        case 1  => "1"
        case -1 => "-1"
      }
    } else {
      val strRep = (this.underlying map (pair => pair._1 + "^" + pair._2) mkString (" * "))
      signum match {
        case 0  => "0"
        case 1  => strRep
        case -1 => "-1 * " + strRep
      }
    }
    str
  }

  private def mergePowers(main: Map[Int, BigFrac], other: Map[Int, BigFrac]): Map[Int, BigFrac] = {
    if (other.isEmpty) main
    else {
      val (headPrime, headPow) = other.head
      val updatedMain = Product.changePower(main, headPrime, headPow)
      mergePowers(updatedMain, other.tail)
    }
  }

  private def extractCommonFactors(one: Map[Int, BigFrac],
                                   two: Map[Int, BigFrac]): (Map[Int, BigFrac], Map[Int, BigFrac], Map[Int, BigFrac]) = {
    def extract(keys: Seq[Int],
                acc: Map[Int, BigFrac],
                one: Map[Int, BigFrac],
                two: Map[Int, BigFrac]): (Map[Int, BigFrac], Map[Int, BigFrac], Map[Int, BigFrac]) = {
      if (keys.isEmpty) (acc, one, two)
      else {
        val key = keys.head
        val min = one(key) min two(key)
        val newOne = Product.changePower(one, key, -min)
        val newTwo = Product.changePower(two, key, -min)
        extract(keys.tail, acc + (key -> min), newOne, newTwo)
      }
    }
    val commonKeys = one.keys.toSeq intersect two.keys.toSeq
    extract(commonKeys, Map.empty, one, two)
  }
}

object Product {

  val ZERO = new Product(0, Map.empty)
  val ONE = new Product(1, Map.empty)
  val MINUS_ONE = new Product(-1, Map.empty)

  def apply(v: Int): Product = {
    if (v == 0) ZERO
    else if (v == 1) ONE
    else if (v == -1) MINUS_ONE
    else new Product(v.signum, factorize(v.abs))
  }

  def apply(v: BigInt): Product = {
    require(v.isValidInt, "Value is too large")
    Product(v.toInt)
  }

  def apply(v: BigFrac): Product = {
    require(v.num.isValidInt, "Numerator is too large")
    require(v.den.isValidInt, "Denomiator is too large")
    if (v.num == 0) ZERO
    else {
      val numPows = factorize(v.num.toInt.abs)
      val denPows = factorize(v.den.toInt.abs)
      val powers = denPows.foldLeft(numPows) {
        case (acc, (factor, power)) => changePower(acc, factor, -power)
      }
      Product(v.signum, powers)
    }
  }

  def factorize(v: Int): Map[Int, BigFrac] = {
    require(v > 0, "Can only factorize positive numbers")
    import MathUtils._
    def rec(v: Int, primes: Seq[Int], acc: Map[Int, BigFrac]): Map[Int, BigFrac] =
      if (primes.isEmpty) // v is prime itself
        incPower(acc, v)
      else if (v == 1) acc
      else if (v < primes.head) throw new RuntimeException("Something went wrong!")
      else {
        val prime = primes.head
        val (div, rem) = (v / prime, v % prime)
        if (rem == 0)
          rec(div, primes, incPower(acc, prime))
        else
          rec(v, primes.tail, acc)
      }
    rec(v, primesUpTo(sqrt(v)), Map.empty) filter (_._1 != 1)
  }

  def incPower(map: Map[Int, BigFrac], key: Int): Map[Int, BigFrac] =
    changePower(map, key, 1)

  def changePower(map: Map[Int, BigFrac], key: Int, diff: BigFrac): Map[Int, BigFrac] = {
    val oldVal = map.getOrElse(key, BigFrac.ZERO)
    if (oldVal + diff == 0) map - key
    else map updated (key, oldVal + diff)
  }

  implicit object ProductNumeric extends Numeric[Product] {
    override def compare(a: Product, b: Product) = a compare b
    override def plus(x: Product, y: Product): Product = x + y
    override def minus(x: Product, y: Product): Product = x - y
    override def times(x: Product, y: Product): Product = x * y
    override def negate(x: Product): Product = -x
    override def fromInt(x: Int): Product = Product(x)
    override def toInt(x: Product): Int = x.intValue
    override def toLong(x: Product): Long = x.longValue
    override def toFloat(x: Product): Float = x.floatValue
    override def toDouble(x: Product): Double = x.doubleValue
  }

}