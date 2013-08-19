package org.newtonpolyhedron.entity

import org.newtonpolyhedron._
import org.apache.commons.math3.FieldElement
import scala.math.ScalaNumber
import scala.math.ScalaNumericConversions
import org.apache.commons.math3.Field

// How to raise complex number to a fractional power?
case class Complex(val r: Product, i: Product)
    extends ScalaNumber
    with ScalaNumericConversions
    with FieldElement[Complex]
    with Ordered[Complex]
    with Serializable {

  def isReal = i.isZero

  def unary_- = Complex(-this.r, this.i)
  def +(that: Complex) = Complex(this.r + that.r, this.i + that.i)
  def -(that: Complex) = Complex(this.r - that.r, this.i - that.i)
  def *(that: Complex) = {
    val (a, b, c, d) = (this.r, this.i, that.r, that.i)
    Complex(a * c - b * d, b * c + a * d)
  }
  def *(that: Int) = Complex(this.r * that, this.i * that)

  def /(that: Complex) = {
    val (a, b, c, d) = (this.r, this.i, that.r, that.i)
    val den = (c * c + d * d)
    val realNum = a * c + b * d
    val imgnNum = b * c - a * d
    Complex(realNum / den, imgnNum / den)
  }

  def inv = {
    val (a, b) = (this.r, this.i)
    val den = (a * a + b * b)
    Complex(a / den, -b / den)
  }

  def pow(n: Int): Complex =
    if (n == 0) Complex.ONE
    else if (n < 0) this.inv pow (-n)
    else {
      val (real2, imgn2) = (0 to n).foldLeft((Product.ZERO, Product.ZERO)) {
        case ((racc, iacc), k) => {
          val coeff = (n choose k)
          val pReal = r pow (n - k)
          val pImgn = i pow k
          val mul = pReal * pImgn * coeff
          k match {
            case k if (k == 0 || k % 4 == 0)       => (racc + mul, iacc)
            case k if (k % 2 == 0)                 => (racc - mul, iacc)
            case k if (k == 1 || (k - 1) % 4 == 0) => (racc, iacc + mul)
            case _                                 => (racc, iacc - mul)
          }
        }
      }
      Complex(real2, imgn2)
    }

  def abs: Product =
    (r.pow(2) + i.pow(2)).sqrt

  override def compare(that: Complex): Int = {
    val c1 = this.r compare that.r
    if (c1 != 0) c1
    else this.i compare that.i
  }

  override def add(that: Complex) = this + that
  override def subtract(that: Complex) = this - that
  override def multiply(that: Complex) = this * that
  override def multiply(that: Int) = this * that
  override def divide(that: Complex) = this / that
  override def reciprocal = this.inv
  override def negate = -this
  override def getField = Complex.ComplexField

  private def enforceReal: this.type = if (isReal) this else throw new IllegalStateException("Has imaginary part")
  override def isWhole = this.isReal
  override def underlying = (this.r, this.i)
  override def intValue = enforceReal.r.intValue
  override def longValue = enforceReal.r.longValue
  override def floatValue = enforceReal.r.floatValue
  override def doubleValue = enforceReal.r.doubleValue

  override def toString = s"($r) + i*($i)"
}

object Complex {
  val ZERO = Complex(Product.ZERO, Product.ZERO)
  val ONE = Complex(Product.ONE, Product.ZERO)

  def apply(r: Long, i: Long): Complex =
    Complex(Product(r), Product(i))

  def apply(r: BigFrac, i: BigFrac): Complex =
    Complex(Product(r), Product(i))

  implicit object ComplexNumeric extends Numeric[Complex] {
    override def compare(a: Complex, b: Complex) = a compare b
    override def plus(x: Complex, y: Complex): Complex = x + y
    override def minus(x: Complex, y: Complex): Complex = x - y
    override def times(x: Complex, y: Complex): Complex = x * y
    override def negate(x: Complex): Complex = -x
    override def fromInt(x: Int): Complex = Complex(x, 1)
    override def toInt(x: Complex): Int = x.toInt
    override def toLong(x: Complex): Long = x.toInt
    override def toFloat(x: Complex): Float = x.toInt
    override def toDouble(x: Complex): Double = x.toInt
  }

  implicit object ComplexField extends Field[Complex] {
    def getZero = Complex.ZERO
    def getOne = Complex.ONE
    def getRuntimeClass = classOf[Complex]
  }
}