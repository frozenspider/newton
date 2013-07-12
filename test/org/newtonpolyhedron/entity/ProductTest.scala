package org.newtonpolyhedron.entity

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ProductTest extends FunSuite {

  private def p(i: Int) = Product(i)
  private def p(n: Int, d: Int) = Product(BigFrac(n, d))
  private def bf(n: Int, d: Int) = BigFrac(n, d)

  test("standard corner cases") {
    assert(Product.ZERO.intValue === 0)
    assert(Product.ONE.intValue === 1)
    assert(Product.MINUS_ONE.intValue === -1)
    assert((Product.ONE * Product.MINUS_ONE).intValue === -1)
    assert((Product.ZERO * Product.MINUS_ONE).intValue === 0)
    assert((Product.MINUS_ONE * p(3)).intValue === -3)
  }

  test("parsing random numbers") {
    assert(p(0).intValue === 0)
    assert(p(1).intValue === 1)
    assert(p(-1).intValue === -1)
    assert(p(2).intValue === 2)
    assert(p(3).intValue === 3)
    assert(p(4).intValue === 4)
    assert(p(10).intValue === 10)
    assert(p(100500).intValue === 100500)
    assert(p(-100500).intValue === -100500)
  }

  test("parsing fraction") {
    assert(p(0, 1).intValue === 0)
    assert(p(0, 1).fracValue === BigFrac.ZERO)
    assert(p(1, 2).fracValue === bf(1, 2))
    assert(p(3, 8).fracValue === bf(3, 8))
    assert(p(3, 8).underlying === p(6, 16).underlying)
  }

  test("multiplication") {
    assert((p(0) * p(10)).intValue === 0)
    assert((p(10) * p(0)).intValue === 0)
    assert((p(10) * p(0)).intValue != 10)
    assert((p(-10) * p(-20)).intValue === 200)
    assert((p(-10) * p(7)).intValue === -70)
    assert((p(1, 3) * p(3)).intValue === 1)
    assert((p(1, 3) * p(3)).fracValue === 1)
    assert((p(3, 4) * p(8)).intValue === 6)
    assert((p(3, 4) * p(8)).fracValue === 6)
  }

  test("division") {
    assert((p(0) / p(10)).intValue === 0)
    assert((p(-10) / p(-20)).fracValue === bf(1, 2))
    assert((p(-10) / p(7)).fracValue === bf(-10, 7))
    assert((p(1, 3) / p(3)).fracValue === bf(1, 9))
    assert((p(1, 3) / p(1, 3)).fracValue === 1)
    assert((p(3, 4) / p(1, 8)).fracValue === 6)
  }

  test("power") {
    assert((p(0) pow 3).intValue === 0)
    assert((p(1) pow 3).intValue === 1)
    assert((p(0) pow 0).intValue === 1)
    assert((p(1) pow 1).intValue === 1)
    assert((p(1) pow 10).intValue === 1)
    assert((p(10) pow 1).intValue === 10)
    assert((p(10) pow 0).intValue === 1)
    assert((p(2) pow 16).intValue === 65536)
    assert((p(6) pow 3).intValue === 216)
    assert((p(8) pow 6).intValue === 262144)
    assert((p(1, 2) pow 3).fracValue === bf(1, 8))
    assert((p(3) pow -2).fracValue === bf(1, 9))
  }

  test("addition, subtraction") {
    assert((p(0) + p(0)).intValue === 0)
    assert((p(0) + p(1)).intValue === 1)
    assert((p(-1) + p(0)).intValue === -1)
    assert((p(-1) + p(1)).intValue === 0)
    assert((p(6) + p(7)).intValue === 13)
    assert((p(30) + p(12)).intValue === 42)
    assert((p(30) + p(-12)).intValue === 18)
    assert((p(-30) + p(12)).intValue === -18)
    assert((-p(30) + p(12)).intValue === -18)
    assert((p(12) - p(30)).intValue === -18)
    assert((-p(0) - -p(0)).intValue === 0)
  }

  test("checking divisors") {
    assert(p(3).underlying === Map(3 -> 1))
    assert(p(4).underlying === Map(2 -> 2))
    assert(p(10).underlying === Map(2 -> 1, 5 -> 1))
    assert(p(12).underlying === Map(2 -> 2, 3 -> 1))
    assert(p(17).underlying === Map(17 -> 1))
    assert(p(51).underlying === Map(3 -> 1, 17 -> 1))
    assert(p(1, 32).underlying === Map(2 -> -5))
  }
}