package org.newtonpolyhedron.entity

import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.vector.FracMathVec

case class Term(val coeff: Product, val powers: FracMathVec) {
  def withCoeff(coeff: Product) = new Term(coeff, powers)
}

object Term {
  def apply(pair: (Product, FracMathVec)): Term = Term(pair._1, pair._2)
  def zero(dim: Int): Term = Term(Product.ZERO, FracMathVec.zero(dim))
}