package org.newtonpolyhedron.utils

import scala.collection.immutable.SortedSet

object MathUtils {
  def primesUpTo(max: Int): Seq[Int] = {
    val init = SortedSet((2 to max): _*)
    (2 to max).foldLeft(init) { (primes, v) =>
      if (primes(v)) primes -- (v * v to max by v)
      else primes
    }.toSeq
  }
}
