package org.newtonpolyhedron.math

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.Product

object Polynomials {
  implicit class RichPolynomial(p: Polynomial) {
    /** Substitute variables in all terms with the given values */
    def withValues(vs: Seq[Product]): Seq[Product] = {
      val substitutedTerms = p map { term =>
        require(vs.size == term.powers.dim, s"Dimension of term $term doesn't equal ${vs.size}")
        val powered = vs zip term.powers.elements map {
          case (value, power) => 
            val v = value pow power
            v
        }
        term.coeff * powered.product
      }
      substitutedTerms
    }

    /** Substitute variables in all terms with the given values and sum the result */
    def totalWithValues(vs: Seq[Product]): Product = {
      withValues(vs).sum
    }

    /** Substitute variables in all terms with the given values and sum the result as far as possible */
    def totalWithValuesNonStrict(vs: Seq[Product]): Seq[Product] = {
      reduceSum(withValues(vs))
    }

    /** Substitute variables in all terms with the given values and checks the resulting sum for being zero */
    def isZeroWithValues(vs: Seq[Product]): Boolean = {
      val substituted = withValues(vs)
      val reduced = reduceSum(substituted)
      reduced.tail.isEmpty && reduced.head.isZero
    }
  }

  private def isSummable(p1: Product, p2: Product): Boolean =
    try {
      p1 + p2
      true
    } catch {
      case ex: IllegalArgumentException => false
    }

  private def reduceSum(sum: Seq[Product]): Seq[Product] = {
    def reduceSumInner(accUnreducible: Seq[Product], remainings: Seq[Product]): Seq[Product] =
      remainings match {
        case e if e.isEmpty =>
          accUnreducible
        case x +: xs =>
          val xsPairs = xs mapWithIndex ((xx, i) => (xx, xs.patch(i, Nil, 1)))
          val stepOption = xsPairs.collectFirst {
            case (potentialPair, restWithoutSelected) if isSummable(x, potentialPair) =>
              (x + potentialPair, restWithoutSelected)
          }
          stepOption match {
            case Some((sum, rest)) => reduceSumInner(accUnreducible, sum +: rest)
            case None              => reduceSumInner(accUnreducible :+ x, xs)
          }
      }
    reduceSumInner(Nil, sum).toIndexedSeq
  }

  def main(args: Array[String]): Unit = {
    import org.newtonpolyhedron.entity.BigFrac
    //sqrt(3) - sqrt(6) - sqrt(3)(5) + sqrt(3)
    val r = reduceSum(Seq(
      Product(3).sqrt,
      -Product(3).sqrt
    ))
    println(r map (_.toStructuredString))
  }
}
