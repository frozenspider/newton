package org.newtonpolyhedron.math.internal

import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.math.MathProcessor
import org.newtonpolyhedron.math.Imports._
import spire.math.Rational

/**
 * @author FS
 */
class InternalMathProcessor extends MathProcessor[InternalNumber] {
  private implicit val mp = this

  override def zero: InternalNumber =
    Product.zero

  override def one: InternalNumber =
    Product.one

  override def isZero(x: InternalNumber): Boolean = x match {
    case x: Product => x.signum == 0
  }

  override def compare(x: InternalNumber, y: InternalNumber): Int = (x, y) match {
    case (x: Product, y: Product) => x.toRational compare y.toRational
  }

  override def negate(x: InternalNumber): InternalNumber = x match {
    case x: Product => new Product(-x.signum, x.underlying)
  }

  override def inverse(x: InternalNumber): InternalNumber =
    one / x

  override def add(x: InternalNumber, y: InternalNumber): InternalNumber = x match {
    case x: Product => y match {
      case y: Product =>
        val (commonFactors, thisFactors, thatFactors) = extractCommonFactors(x.underlying, y.underlying)
        val common = new Product(1, commonFactors)

        val part1 = new Product(x.signum, thisFactors)
        val part2 = new Product(y.signum, thatFactors)
        require(part1.isRational && part2.isRational, "Can't sum these non-rational products (not in general case)")
        val partSum = Product(part1.toRational + part2.toRational)
        common * partSum
    }
  }

  override def subtract(x: InternalNumber, y: InternalNumber): InternalNumber = add(x, negate(y))

  override def multiply(x: InternalNumber, y: InternalNumber): InternalNumber = x match {
    case x: Product => y match {
      case y: Product =>
        if (x.signum == 0 || y.signum == 0) Product.zero
        else new Product(x.signum * y.signum, mergePowers(x.underlying, y.underlying))
    }
  }

  override def divide(x: InternalNumber, y: InternalNumber): InternalNumber = x match {
    case x: Product => y match {
      case y: Product =>
        require(y.signum != 0, "Divide by zero")
        if (x.signum == 0) Product.zero
        else new Product(x.signum * y.signum, mergePowers(x.underlying, y.underlying map (p => (p._1, -p._2))))
    }
  }

  override def raise(x: InternalNumber, y: InternalNumber): InternalNumber = x match {
    case x: Product => y match {
      case y: Product if y.isRational =>
        val yr = y.toRational
        if (x.signum == 0) yr match {
          case y if y == 0 => Product.one
          case y if y > 0  => Product.zero
          case _           => throw new IllegalArgumentException("Can't raise zero to negative power")
        }
        else {
          val newSignum = if (yr == 0 || yr.numerator % 2 == 0) 1 else x.signum
          val newUnderlying = x.underlying map (e => (e._1, e._2 * yr)) filter (_._2 != 0)
          new Product(newSignum, newUnderlying)
        }
      case _ => throw new IllegalArgumentException("Product can only be raised to rational power")
    }
  }

  override def proot(x: InternalNumber, y: InternalNumber): InternalNumber = x match {
    case x: Product => y match {
      case y: Product if y.isRational =>
        x ** inverse(y)
      case _ => throw new IllegalArgumentException("Product can only be raised to rational power")
    }
  }

  override def fromInt(x: Int): InternalNumber = InternalMathProcessor.int2product(x)

  override def fromBigInt(x: BigInt): InternalNumber = InternalMathProcessor.bint2product(x)

  override def fromRational(x: Rational): InternalNumber = InternalMathProcessor.rat2product(x)

  override def toInt(x: InternalNumber): Int = x match {
    case x: Product => x.toRational.toInt
  }

  override def toLong(x: InternalNumber): Long = x match {
    case x: Product => x.toRational.toLong
  }

  override def toDouble(x: InternalNumber): Double = x match {
    case x: Product =>
      if (x.signum == 0) 0.0d else {
        val folded = (x.underlying foldLeft 1.0d) {
          case (acc, (v, p)) => acc * math.pow(v, p.doubleValue)
        }
        x.signum * folded
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

  private def mergePowers(main: Map[Int, Rational], other: Map[Int, Rational]): Map[Int, Rational] = {
    if (other.isEmpty) main
    else {
      val (headPrime, headPow) = other.head
      val updatedMain = Product.changePower(main, headPrime, headPow)
      mergePowers(updatedMain, other.tail)
    }
  }
}

object InternalMathProcessor {
  implicit def int2product(x: Int): Product = Product(x)
  implicit def bint2product(x: BigInt): Product = Product(x)
  implicit def rat2product(x: Rational): Product = Product(x)
}
