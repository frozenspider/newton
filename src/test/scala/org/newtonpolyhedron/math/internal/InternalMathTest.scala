package org.newtonpolyhedron.math.internal

import org.newtonpolyhedron.test._
import org.newtonpolyhedron.NewtonImports._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import spire.math.Rational

@RunWith(classOf[JUnitRunner])
class InternalMathTest
    extends FunSuite
    with InternalMathProcessorMixin {

  test("standard corner cases") {
    val (zero, one, minusOne) = (mp.zero, mp.one, -mp.one)
    assert(zero.toInt === 0)
    assert(one.toInt === 1)
    assert(minusOne.toInt === -1)
    assert((one * minusOne).toInt === -1)
    assert((zero * minusOne).toInt === 0)
    assert((minusOne * n(3)).toInt === -3)
  }

  test("parsing random numbers") {
    assert(n(0).toInt === 0)
    assert(n(1).toInt === 1)
    assert(n(-1).toInt === -1)
    assert(n(2).toInt === 2)
    assert(n(3).toInt === 3)
    assert(n(4).toInt === 4)
    assert(n(10).toInt === 10)
    assert(n(100500).toInt === 100500)
    assert(n(-100500).toInt === -100500)
  }

  test("parsing fraction") {
    assert(n(0, 1).toInt === 0)
    assert(n(0, 1).toRational === Rational.zero)
    assert(n(1, 2).toRational === frac(1, 2))
    assert(n(3, 8).toRational === frac(3, 8))
    assert(n(3, 8).underlying === n(6, 16).underlying)
  }

  test("multiplication") {
    assert((n(0) * n(10)).toInt === 0)
    assert((n(10) * n(0)).toInt === 0)
    assert((n(10) * n(0)).toInt != 10)
    assert((n(-10) * n(-20)).toInt === 200)
    assert((n(-10) * n(7)).toInt === -70)
    assert((n(1, 3) * n(3)).toInt === 1)
    assert((n(1, 3) * n(3)).toRational === 1)
    assert((n(3, 4) * n(8)).toInt === 6)
    assert((n(3, 4) * n(8)).toRational === 6)
  }

  test("division") {
    assert((n(0) / n(10)).toInt === 0)
    assert((n(-10) / n(-20)).toRational === frac(1, 2))
    assert((n(-10) / n(7)).toRational === frac(-10, 7))
    assert((n(1, 3) / n(3)).toRational === frac(1, 9))
    assert((n(1, 3) / n(1, 3)).toRational === 1)
    assert((n(3, 4) / n(1, 8)).toRational === 6)
  }

  test("power, integer") {
    assert((n(0) ** 0).toInt === 1)
    assert((n(0) ** 3).toInt === 0)
    assert((n(0) ** 10).toInt === 0)
    assert((n(1) ** 1).toInt === 1)
    assert((n(1) ** 3).toInt === 1)
    assert((n(1) ** 10).toInt === 1)
    assert((n(10) ** 0).toInt === 1)
    assert((n(10) ** 1).toInt === 10)
    assert((n(-10) ** 0).toInt === 1)
    assert((n(2) ** 16).toInt === 65536)
    assert((n(6) ** 3).toInt === 216)
    assert((n(8) ** 6).toInt === 262144)
    assert((n(1, 2) ** 3).toRational === frac(1, 8))
    assert((n(3) ** -2).toRational === frac(1, 9))
  }

  test("power, fractional, precise") {
    assert((n(0) ** frac(3, 1)).toInt === 0)
    assert((n(1) ** frac(3, 1)).toInt === 1)
    assert((n(0) ** frac(0, 1)).toInt === 1)
    assert((n(1) ** frac(1, 1)).toInt === 1)
    assert((n(1) ** frac(10, 1)).toInt === 1)
    assert((n(4) ** frac(1, 2)).toInt === 2)
    assert((n(65536) ** frac(1, 4)).toInt === 16)
    assert((n(8) ** frac(1, 3)).toInt === 2)
    assert((n(8) ** frac(-1, 3)).toRational === frac(1, 2))
  }

  test("power, fractional, appx") {
    assert((n(8) ** frac(1, 2)).toDouble =~= 2.828427125)
    intercept[ArithmeticException] {
      (n(8) ** frac(1, 2)).toRational
    }
  }

  test("power, product - integer and fractional") {
    assert((n(0) ** n(0)).toInt === 1)
    assert((n(3) ** n(-2)).toRational === frac(1, 9))
    assert((n(-10) ** n(0)).toInt === 1)
    assert((n(2) ** n(16)).toInt === 65536)
    assert((n(65536) ** n(1, 4)).toInt === 16)
    assert((n(8) ** n(1, 3)).toInt === 2)
    assert((n(8) ** n(-1, 3)).toRational === frac(1, 2))
  }

  test("addition, subtraction") {
    assert((n(0) + n(0)).toInt === 0, "")
    assert((n(0) + n(1)).toInt === 1)
    assert((n(-1) + n(0)).toInt === -1)
    assert((n(-1) + n(1)).toInt === 0)
    assert((n(6) + n(7)).toInt === 13)
    assert((n(30) + n(12)).toInt === 42)
    assert((n(30) + n(-12)).toInt === 18)
    assert((n(-30) + n(12)).toInt === -18)
    assert((-n(30) + n(12)).toInt === -18)
    assert((n(12) - n(30)).toInt === -18)
    assert((-n(0) - -n(0)).toInt === 0)
  }

  test("checking divisors") {
    assert(n(3).underlying === Map(3 -> 1))
    assert(n(4).underlying === Map(2 -> 2))
    assert(n(10).underlying === Map(2 -> 1, 5 -> 1))
    assert(n(12).underlying === Map(2 -> 2, 3 -> 1))
    assert(n(17).underlying === Map(17 -> 1))
    assert(n(51).underlying === Map(3 -> 1, 17 -> 1))
    assert(n(1, 1).underlying === Map.empty)
    assert(n(1, 32).underlying === Map(2 -> -5))
  }
}
