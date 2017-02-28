package org.newtonpolyhedron.entity

import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import spire.math.Rational

@RunWith(classOf[JUnitRunner])
class ProductTest extends FunSuite {

  private def p(i: Int) = Product(i)
  private def p(n: Int, d: Int) = Product(Rational(n, d))

  test("standard corner cases") {
    assert(Product.ZERO.toInt === 0)
    assert(Product.ONE.toInt === 1)
    assert(Product.MINUS_ONE.toInt === -1)
    assert((Product.ONE * Product.MINUS_ONE).toInt === -1)
    assert((Product.ZERO * Product.MINUS_ONE).toInt === 0)
    assert((Product.MINUS_ONE * p(3)).toInt === -3)
  }

  test("parsing random numbers") {
    assert(p(0).toInt === 0)
    assert(p(1).toInt === 1)
    assert(p(-1).toInt === -1)
    assert(p(2).toInt === 2)
    assert(p(3).toInt === 3)
    assert(p(4).toInt === 4)
    assert(p(10).toInt === 10)
    assert(p(100500).toInt === 100500)
    assert(p(-100500).toInt === -100500)
  }

  test("parsing fraction") {
    assert(p(0, 1).toInt === 0)
    assert(p(0, 1).toRational === Rational.zero)
    assert(p(1, 2).toRational === frac(1, 2))
    assert(p(3, 8).toRational === frac(3, 8))
    assert(p(3, 8).underlying === p(6, 16).underlying)
  }

  test("multiplication") {
    assert((p(0) * p(10)).toInt === 0)
    assert((p(10) * p(0)).toInt === 0)
    assert((p(10) * p(0)).toInt != 10)
    assert((p(-10) * p(-20)).toInt === 200)
    assert((p(-10) * p(7)).toInt === -70)
    assert((p(1, 3) * p(3)).toInt === 1)
    assert((p(1, 3) * p(3)).toRational === 1)
    assert((p(3, 4) * p(8)).toInt === 6)
    assert((p(3, 4) * p(8)).toRational === 6)
  }

  test("division") {
    assert((p(0) / p(10)).toInt === 0)
    assert((p(-10) / p(-20)).toRational === frac(1, 2))
    assert((p(-10) / p(7)).toRational === frac(-10, 7))
    assert((p(1, 3) / p(3)).toRational === frac(1, 9))
    assert((p(1, 3) / p(1, 3)).toRational === 1)
    assert((p(3, 4) / p(1, 8)).toRational === 6)
  }

  test("power, integer") {
    assert((p(0) pow 0).toInt === 1)
    assert((p(0) pow 3).toInt === 0)
    assert((p(0) pow 10).toInt === 0)
    assert((p(1) pow 1).toInt === 1)
    assert((p(1) pow 3).toInt === 1)
    assert((p(1) pow 10).toInt === 1)
    assert((p(10) pow 0).toInt === 1)
    assert((p(10) pow 1).toInt === 10)
    assert((p(-10) pow 0).toInt === 1)
    assert((p(2) pow 16).toInt === 65536)
    assert((p(6) pow 3).toInt === 216)
    assert((p(8) pow 6).toInt === 262144)
    assert((p(1, 2) pow 3).toRational === frac(1, 8))
    assert((p(3) pow -2).toRational === frac(1, 9))
  }

  test("power, fractional, precise") {
    assert((p(0) pow frac(3, 1)).toInt === 0)
    assert((p(1) pow frac(3, 1)).toInt === 1)
    assert((p(0) pow frac(0, 1)).toInt === 1)
    assert((p(1) pow frac(1, 1)).toInt === 1)
    assert((p(1) pow frac(10, 1)).toInt === 1)
    assert((p(4) pow frac(1, 2)).toInt === 2)
    assert((p(65536) pow frac(1, 4)).toInt === 16)
    assert((p(8) pow frac(1, 3)).toInt === 2)
    assert((p(8) pow frac(-1, 3)).toRational === frac(1, 2))
  }

  test("power, fractional, appx") {
    assert((p(8) pow frac(1, 2)) =~= 2.828427125)
    intercept[ArithmeticException] {
      (p(8) pow frac(1, 2)).toRational
    }
  }

  test("addition, subtraction") {
    assert((p(0) + p(0)).toInt === 0)
    assert((p(0) + p(1)).toInt === 1)
    assert((p(-1) + p(0)).toInt === -1)
    assert((p(-1) + p(1)).toInt === 0)
    assert((p(6) + p(7)).toInt === 13)
    assert((p(30) + p(12)).toInt === 42)
    assert((p(30) + p(-12)).toInt === 18)
    assert((p(-30) + p(12)).toInt === -18)
    assert((-p(30) + p(12)).toInt === -18)
    assert((p(12) - p(30)).toInt === -18)
    assert((-p(0) - -p(0)).toInt === 0)
  }

  test("checking divisors") {
    assert(p(3).underlying === Map(3 -> 1))
    assert(p(4).underlying === Map(2 -> 2))
    assert(p(10).underlying === Map(2 -> 1, 5 -> 1))
    assert(p(12).underlying === Map(2 -> 2, 3 -> 1))
    assert(p(17).underlying === Map(17 -> 1))
    assert(p(51).underlying === Map(3 -> 1, 17 -> 1))
    assert(p(1, 1).underlying === Map.empty)
    assert(p(1, 32).underlying === Map(2 -> -5))
  }
}
