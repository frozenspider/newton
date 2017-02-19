package org.newtonpolyhedron.utils

import scala.collection.immutable.SortedSet

object MathUtils {
  def sqrt(number: BigInt): BigInt = {
    def next(n: BigInt, i: BigInt): BigInt =
      (n + i / n) >> 1
    val one = BigInt(1)
    var n = one
    var n1 = next(n, number)
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
    while (n1 * n1 > number) {
      n1 -= one
    }
    n1
  }

  def sqrt(number: Int): Int = {
    def next(n: Int, i: Int): Int =
      (n + i / n) >> 1
    val one = 1
    var n = one
    var n1 = next(n, number)
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
    while (n1 * n1 > number) {
      n1 -= one
    }
    n1
  }

  def primesUpTo(max: Int): Seq[Int] = {
    val init = SortedSet((2 to max): _*)
    (2 to max).foldLeft(init) { (primes, v) =>
      if (primes(v)) primes -- (v * v to max by v)
      else primes
    }.toSeq
  }
}
