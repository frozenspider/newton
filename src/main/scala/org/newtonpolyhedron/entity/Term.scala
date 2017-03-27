package org.newtonpolyhedron.entity

import org.newtonpolyhedron.NewtonImports._

case class Term[N <: MPNumber](val coeff: N, val powers: NumVec[N]) {

  def withCoeff(coeff: N) = new Term(coeff, powers)

  def withPowers(powers: NumVec[N]) = new Term(coeff, powers)

  def mapPowers(f: NumVec[N] => NumVec[N]) = new Term(coeff, f(powers))

  def mapEachPower(f: N => N) = new Term(coeff, powers map f)

  def *(that: N)(implicit mp: MathProcessor[N]): Term[N] =
    new Term(this.coeff * that, this.powers)

  def *(that: Term[N])(implicit mp: MathProcessor[N]): Term[N] =
    new Term(this.coeff * that.coeff, this.powers + that.powers)

  def /(that: N)(implicit mp: MathProcessor[N]): Term[N] =
    new Term(this.coeff / that, this.powers)

  def /(that: Term[N])(implicit mp: MathProcessor[N]): Term[N] =
    new Term(this.coeff / that.coeff, this.powers - that.powers)

  def unary_-(implicit mp: MathProcessor[N]): Term[N] = withCoeff(-coeff)

  def **(that: Int)(implicit mp: MathProcessor[N]): Term[N] =
    new Term(this.coeff ** mp.fromInt(that), this.powers * mp.fromInt(that))

  def **(that: Rational)(implicit mp: MathProcessor[N]): Term[N] =
    new Term(this.coeff ** mp.fromRational(that), this.powers * mp.fromRational(that))

  def root(that: Int)(implicit mp: MathProcessor[N]): Term[N] =
    this ** Rational(1, that)
}

object Term {
  def apply[N <: MPNumber](pair: (N, NumVec[N])): Term[N] = Term(pair._1, pair._2)
  def zero[N <: MPNumber](dim: Int)(implicit mp: MathProcessor[N]): Term[N] =
    Term(mp.zero, IndexedSeq.fill(dim)(mp.zero))
  def one[N <: MPNumber](dim: Int)(implicit mp: MathProcessor[N]): Term[N] =
    Term(mp.one, IndexedSeq.fill(dim)(mp.zero))
}
