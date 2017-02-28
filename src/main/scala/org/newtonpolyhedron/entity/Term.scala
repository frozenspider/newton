package org.newtonpolyhedron.entity

import org.newtonpolyhedron.entity.vector.VectorImports._
import spire.math.Rational

case class Term(val coeff: Product, val powers: FracVec) {

  def withCoeff(coeff: Product) = new Term(coeff, powers)

  def withPowers(powers: FracVec) = new Term(coeff, powers)

  def mapPowers(f: FracVec => FracVec) = new Term(coeff, f(powers))

  def mapEachPower(f: Rational => Rational) = new Term(coeff, powers map f)

  def *(that: Term): Term =
    new Term(this.coeff * that.coeff, this.powers + that.powers)

  def *(that: Product): Term =
    new Term(this.coeff * that, this.powers)

  def /(that: Term): Term =
    new Term(this.coeff / that.coeff, this.powers - that.powers)

  def /(that: Product): Term =
    new Term(this.coeff / that, this.powers)

  def unary_- : Term = withCoeff(-coeff)

  def pow(that: Int): Term =
    new Term(this.coeff pow that, this.powers * that)

  def pow(that: Rational): Term =
    new Term(this.coeff pow that, this.powers * that)

  def powInv(that: Int): Term =
    this pow Rational(1, that)
}

object Term {
  def apply(pair: (Product, Vector[Rational])): Term = Term(pair._1, pair._2)
  def zero(dim: Int): Term = Term(Product.ZERO, IndexedSeq.fill(dim)(Rational.zero))
  def one(dim: Int): Term = Term(Product.ONE, IndexedSeq.fill(dim)(Rational.zero))
}
