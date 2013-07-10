package org.newtonpolyhedron.entity

import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.vector.IntMathVec

case class Term(val coeff: Product, val powers: IntMathVec) {
  def withCoeff(coeff: Product) = new Term(coeff, powers)
}

object Term {
  def apply(pair: (Product, IntMathVec)): Term = Term(pair._1, pair._2)
  def zero(dim: Int): Term = Term(Product.ZERO, IntMathVec.zero(dim))
}