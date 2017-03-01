package org.newtonpolyhedron.entity

import scala.collection.immutable.ListMap
import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions

import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.MathUtils

import org.fs.utility.Imports._

import spire.math.Rational

/**
 * Represents a number as a product of powers of its prime factors.
 * <p>
 * E.g. 17 will be represented as 17^1, while 12 would be 2^2 * 3^1.
 * <p>
 * Only supports multiplication and power operations.
 */
case class Product(val signum: Int, val underlying: Map[Int, Rational])
    extends ScalaNumber
    with ScalaNumericConversions
    with Ordered[Product] {
  require((-1 to 1) contains signum, "Sign should be -1, 0 or 1")
  require(if (signum == 0) underlying.isEmpty else true, "For zero product, multipliers should be empty")

  def isZero = signum == 0

  def *(that: Product): Product =
    if (this.signum == 0 || that.signum == 0) Product.zero
    else new Product(this.signum * that.signum, mergePowers(this.underlying, that.underlying))
  def *(that: BigInt): Product = this * Product(that)
  def *(that: Rational): Product = this * Product(that)
  def *(that: Int): Product = this * Product(that)

  def /(that: Product): Product = {
    require(that.signum != 0, "Divide by zero")
    if (this.signum == 0) Product.zero
    else new Product(this.signum * that.signum, mergePowers(this.underlying, that.underlying map (p => (p._1, -p._2))))
  }
  def /(that: BigInt): Product = this / Product(that)
  def /(that: Rational): Product = this / Product(that)
  def /(that: Int): Product = this / Product(that)

  def +(that: Product): Product = {
    val (commonFactors, thisFactors, thatFactors) = extractCommonFactors(this.underlying, that.underlying)
    val common = new Product(1, commonFactors)
    val part1 = new Product(this.signum, thisFactors)
    val part2 = new Product(that.signum, thatFactors)
    require(part1.isRational && part2.isRational, "Can't sum these non-rational products (not in general case)")
    val partSum = Product(part1.toRational + part2.toRational)
    val res = common * partSum
    res
  }
  def +(that: Int): Product = this + Product(that)
  def +(that: Rational): Product = this + Product(that)
  def -(that: Product): Product = this + -that
  def -(that: Int): Product = this + Product(-that)
  def -(that: Rational): Product = this + Product(-that)
  def unary_- : Product = new Product(-signum, underlying)
  def abs: Product = this * signum

  def sqrt = this ** Rational(1, 2)
  def ** (p: Rational): Product = {
    if (this.signum == 0) p match {
      case x if x == 0 => Product.one
      case x if x > 0  => Product.zero
      case _           => throw new IllegalArgumentException("Can't raise zero to negative power")
    }
    else {
      val newSignum = if (p == 0 || p.numerator % 2 == 0) 1 else this.signum
      val newUnderlying = this.underlying map (e => (e._1, e._2 * p)) filter (_._2 != 0)
      new Product(newSignum, newUnderlying)
    }
  }
  def ** (p: Int): Product = this ** Rational(p)

  /** Very ineffective! */
  override def compare(that: Product): Int = this.toRational compare that.toRational

  /** Whether or not this product can be represented as a precise fraction value */
  lazy val isRational = {
    // Check that all powers are integers
    underlying forall (_._2.isWhole)
  }
  override def isWhole = isRational && toRational.isWhole
  override def longValue = toRational.toLong
  override def intValue = toRational.toInt
  lazy val toRational =
    if (signum == 0) Rational.zero else {
      if (!isRational) throw new ArithmeticException("Not a valid fraction")
      val folded = (underlying foldLeft Rational.one) {
        case (acc, (v, p)) => acc * (Rational(v) pow p.numerator.toInt)
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

  /**
   * This product represented as a product of base-N roots.
   *
   * @return rational part along with (RootBase -> RootedValue) map
   */
  lazy val rootedForm: (BigInt, Map[Int, Rational]) = {
    import spire.compat._
    val (rationalMultipliers, irrationalMultipliers) = {
      val unzip = (underlying.toSeq collect {
        case (base, power) if power.isValidInt => (Some((base, power.intValue)), None)
        case (base, power) if power < 1        => (None, (Some((base, power))))
        case (base, power)                     => (Some((base, power.toInt)), Some((base, power - power.toSafeLong)))
      }).unzip
      (unzip._1.yieldDefined, unzip._2.yieldDefined)
    }
    val rational = (rationalMultipliers.map(p => BigInt(p._1) pow p._2) :+ BigInt(1)).product
    val irrationalMultipliersGroups = irrationalMultipliers.groupBy(_._2.denominator).toSeq sortBy (_._1)
    val roots = irrationalMultipliersGroups map {
      case (rootBase, irratMultipliers) =>
        (rootBase.intValue, (irratMultipliers.map({
          case (base, power) =>
            val multipliedPower = power * rootBase
            assert(multipliedPower.isValidInt)
            Rational(base) pow multipliedPower.intValue
        }) :+ Rational.one).product)
    }
    (rational, ListMap(roots: _*))
  }

  override def toString = {
    if (isRational) toRational.toString
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

  private def mergePowers(main: Map[Int, Rational], other: Map[Int, Rational]): Map[Int, Rational] = {
    if (other.isEmpty) main
    else {
      val (headPrime, headPow) = other.head
      val updatedMain = Product.changePower(main, headPrime, headPow)
      mergePowers(updatedMain, other.tail)
    }
  }

  private def extractCommonFactors(one: Map[Int, Rational],
                                   two: Map[Int, Rational]): (Map[Int, Rational], Map[Int, Rational], Map[Int, Rational]) = {
    def extract(keys: Seq[Int],
                acc: Map[Int, Rational],
                one: Map[Int, Rational],
                two: Map[Int, Rational]): (Map[Int, Rational], Map[Int, Rational], Map[Int, Rational]) = {
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

  val zero = new Product(0, Map.empty)
  val one = new Product(1, Map.empty)
  val minusOne = new Product(-1, Map.empty)

  def apply(v: Int): Product = v match {
    case 0  => zero
    case 1  => one
    case -1 => minusOne
    case v  => new Product(v.signum, factorize(v.abs))
  }

  def apply(v: BigInt): Product = {
    require(v.isValidInt, "Value is too large")
    Product(v.toInt)
  }

  def apply(v: Rational): Product = {
    require(v.numerator.isValidInt, "Numerator is too large")
    require(v.denominator.isValidInt, "Denomiator is too large")
    if (v.numerator == 0) zero
    else {
      val numPows = factorize(v.numerator.toInt.abs)
      val denPows = factorize(v.denominator.toInt.abs)
      val powers = denPows.foldLeft(numPows) {
        case (acc, (factor, power)) => changePower(acc, factor, -power)
      }
      Product(v.signum, powers)
    }
  }

  def factorize(v: Int): Map[Int, Rational] = {
    require(v > 0, "Can only factorize positive numbers")
    def rec(v: Int, primes: Seq[Int], acc: Map[Int, Rational]): Map[Int, Rational] =
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
    rec(v, MathUtils.primesUpTo(math.sqrt(v).toInt), Map.empty) filter (_._1 != 1)
  }

  def incPower(map: Map[Int, Rational], key: Int): Map[Int, Rational] =
    changePower(map, key, 1)

  def changePower(map: Map[Int, Rational], key: Int, diff: Rational): Map[Int, Rational] = {
    val oldVal = map.getOrElse(key, Rational.zero)
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
