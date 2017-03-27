package org.newtonpolyhedron.math

/**
 * @author FS
 */
trait Imports {
  type MPNumber = org.newtonpolyhedron.math.MPNumber
  type MathProcessor[N <: MPNumber] = org.newtonpolyhedron.math.MathProcessor[N]

  implicit class RichMPNumber[A, N <: MPNumber](x: A)(implicit mp: MathProcessor[N], conv: A => N) {
    def isZero: Boolean = mp.isZero(x)

    def compare(y: N): Int = mp.compare(x, y)

    def unary_- : N = mp.negate(x)

    def +(y: N): N = mp.add(x, y)

    def -(y: N): N = mp.subtract(x, y)

    def *(y: N): N = mp.multiply(x, y)

    def /(y: N): N = mp.divide(x, y)

    /** Raise a number to a specified power */
    def **(y: N): N = mp.raise(x, y)

    /** Extract the principal root of the given number */
    def proot(y: N): N = mp.proot(x, y)

    def intValue: Int = mp.toInt(x)

    def longValue: Long = mp.toLong(x)

    def doubleValue: Double = mp.toDouble(x)
  }

  implicit def mpNumberNumeric[N <: MPNumber](implicit mp: MathProcessor[N]) =
    new spire.math.Numeric[N] {
      override def compare(a: N, b: N) = new RichMPNumber(a) compare b
      override def plus(x: N, y: N): N = new RichMPNumber(x) + y
      override def minus(x: N, y: N): N = new RichMPNumber(x) - y
      override def times(x: N, y: N): N = new RichMPNumber(x) * y
      override def negate(x: N): N = -new RichMPNumber(x)
      override def fromInt(x: Int): N = mp.fromInt(x)
      override def toInt(x: N): Int = new RichMPNumber(x).intValue
      override def toLong(x: N): Long = new RichMPNumber(x).longValue
      override def toFloat(x: N): Float = toDouble(x).floatValue
      override def toDouble(x: N): Double = new RichMPNumber(x).doubleValue
    }
}

object Imports extends Imports
