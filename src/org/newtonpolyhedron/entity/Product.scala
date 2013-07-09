package org.newtonpolyhedron.entity

import scala.math.ScalaNumber
import org.newtonpolyhedron.utils.MathUtils
import scala.collection.immutable.SortedSet

/**
 * Represents a number as a product of powers of its prime factors.
 * <p>
 * E.g. 17 will be represented as 17^1, while 12 would be 2^2 * 3^1.
 * <p>
 * Only supports multiplication and power operations.
 */
class Product(val signum: Int, val underlying: Map[Int, Int]) extends ScalaNumber {
  require((-1 to 1) contains signum, "Sign should be -1, 0 or 1")
  require(if (signum == 0) underlying.isEmpty else true, "For zero product, powers should be empty")

  def *(that: BigInt): Product = {
    require(that.isValidInt, "Value is too large")
    this * that.toInt
  }

  def *(that: Int): Product = {
    if (this.signum == 0 || that == 0) Product.ZERO
    else {
      val factors = Product.factorize(that.abs)
      new Product(this.signum * that.signum, mergePowers(this.underlying, factors))
    }
  }

  def *(that: Product): Product =
    if (this.signum == 0 || that.signum == 0) Product.ZERO
    else new Product(this.signum * that.signum, mergePowers(this.underlying, that.underlying))

  def pow(p: Int): Product = {
    require(p >= 0, "Can only raise to non-negative powers")
    if (this.signum == 0 && p == 0) Product.ONE
    else new Product(this.signum, this.underlying map (e => (e._1, e._2 * p)) filter (_._2 != 0))
  }

  override def isWhole = true
  override def doubleValue =
    if (signum == 0) 0.0d else signum * toNumber(1.0d)(_ * _)((v, p) => math.pow(v.toDouble, p))
  override def floatValue =
    if (signum == 0) 0.0f else signum * toNumber(1.0f)(_ * _)((v, p) => math.pow(v.toDouble, p).toFloat)
  override def longValue =
    if (signum == 0) 0L else signum * toNumber(1L)(_ * _)((v, p) => math.pow(v.toDouble, p).toLong)
  override def intValue =
    if (signum == 0) 0 else signum * toNumber(1)(_ * _)((v, p) => math.pow(v.toDouble, p).toInt)
  def bigIntValue =
    if (signum == 0) BigInt(0) else signum * toNumber(BigInt(1))(_ * _)(_ pow _)

  override def hashCode = this.underlying.hashCode * 17
  override def toString = {
    val strRep = (this.underlying map (pair => pair._1 + "^" + pair._2) mkString (" * "))
    val str = signum match {
      case 0  => "0"
      case 1  => strRep
      case -1 => "-1 * " + strRep
    }
    "Product(" + str + ")"
  }

  private def toNumber[T](identity: T)(mul: (T, T) => T)(pow: (BigInt, Int) => T): T = {
    (underlying foldLeft identity) {
      case (acc, (v, p)) => mul(acc, pow(v, p))
    }
  }

  private def mergePowers(main: Map[Int, Int], other: Map[Int, Int]): Map[Int, Int] = {
    if (other.isEmpty) main
    else {
      val (headPrime, headPow) = other.head
      val updatedMain = Product.changePower(main, headPrime, headPow)
      mergePowers(updatedMain, other.tail)
    }
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

  def factorize(v: Int): Map[Int, Int] = {
    require(v > 0, "Can only factorize positive numbers")
    import MathUtils._
    def rec(v: Int, primes: Seq[Int], acc: Map[Int, Int]): Map[Int, Int] =
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
    rec(v, primesUpTo(sqrt(v)), Map.empty)
  }

  def incPower(map: Map[Int, Int], key: Int): Map[Int, Int] =
    changePower(map, key, 1)

  def changePower(map: Map[Int, Int], key: Int, diff: Int): Map[Int, Int] =
    map updated (key, map.getOrElse(key, 0) + diff)

}