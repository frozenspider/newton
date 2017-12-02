package org.newtonpolyhedron.utils

import org.newtonpolyhedron.NewtonImports._

import org.newtonpolyhedron.entity.equation._

import spire.math.Rational
import org.newtonpolyhedron.math.internal.Product

object LatexConversion {

  sealed trait LatexStringMixin
  type LatexString = String with LatexStringMixin

  def latex(s: String) = stringToLatex(s)

  private implicit def stringToLatex(s: String) = s.asInstanceOf[LatexString]

  def textToLatex(plaintext: String): LatexString =
    "\\text{" + plaintext + "}"

  def rationalToLatex(rational: Rational): LatexString = {
    if (rational.isWhole) {
      rational.numerator.toString
    } else {
      val rationalVal = "\\frac{" + (rational.numerator.abs) + "}{" + rational.denominator + "}"
      (if (rational.signum < 0) "-" else "") + rationalVal
    }
  }

  def productToLatex(value: Product): LatexString = {
    if (value.isRational) {
      rationalToLatex(value.toRational)
    } else {
      // Irrational case
      val (rational, roots) = value.rootedForm
      val rationalString = if (rational == 1) "" else rational.toString
      val irrationalString = roots.toSeq sortBy (_._1) map {
        case (_, rooted) if rooted == 0 => ""
        case (rootBase, rooted)         => nthRootToLatex(rootBase, rooted)
      }
      "(" + rationalString + irrationalString.mkString + ")"
    }
  }

  def nthRootToLatex(rootBase: Rational, rootedValue: Rational): LatexString = {
    val rootBaseLatex = rationalToLatex(rootBase)
    val rootedValueLatex = rationalToLatex(rootedValue)
    s"\\sqrt[$rootBaseLatex]{$rootedValueLatex}"
  }

  def signToLatex(sign: RelationalSign): LatexString = sign match {
    case RelationalSign.Equals    => "="
    case RelationalSign.Greater   => ">"
    case RelationalSign.GreaterEq => "\\leq"
    case RelationalSign.Less      => "<"
    case RelationalSign.LessEq    => "\\geq"
  }

  private def powersToLatex[N <: MPNumber](varName: String)(pows: Seq[N])(implicit mp: MathProcessor[N]): LatexString = {
    val opts = pows mapWithIndex { (power, i) =>
      if (power.isZero)
        None
      else
        Some(
          variableToLatex(varName, Some(i + 1)) + "^{" + power.toLatexString + "}"
        )
    }
    opts.yieldDefined.mkString
  }

  def variableToLatex(varName: String, index: Option[Int]): LatexString =
    index match {
      case None    => varName
      case Some(i) => varName + "_{" + i + "}"
    }

  def polynomialToLatex[N <: MPNumber](varName: String)(poly: Polynomial[N])(implicit mp: MathProcessor[N]): LatexString = {
    if (poly.isEmpty) {
      ""
    } else {
      val termsSignedStrings = poly map { term =>
        val signum = term.coeff.signum
        (
          signum,
          (term.coeff * mp.fromInt(signum)).toLatexString + powersToLatex(varName)(term.powers)
        )
      }
      signedAbsSeqToLatex(termsSignedStrings)
    }
  }

  def signedAbsSeqToLatex(seq: Seq[(Int, String)]): LatexString = {
    val (headCoeff, headTermAbsString) = seq.head
    val restTermsString = seq.tail.foldLeft("") {
      case (start, (coeff, termAbsString)) if coeff < 0 => start + "-" + termAbsString
      case (start, (coeff, termAbsString))              => start + "+" + termAbsString
    }
    val termsString = headCoeff match {
      case x if x < 0 => "-" + headTermAbsString + restTermsString
      case x          => headTermAbsString + restTermsString
    }
    termsString
  }

  def equationToLatex[N <: MPNumber](varName1: String, varName2: String)(eq: Equation[N])(implicit mp: MathProcessor[N]): LatexString = {
    val lhs = polynomialToLatex(varName1)(eq.lhs)
    val rhs = polynomialToLatex(varName2)(eq.rhs)
    val sign = signToLatex(eq.sign)
    lhs + sign + rhs
  }

  def equationsToLatex[N <: MPNumber](eqs: Equations[N], varName1: String, varName2: String)(implicit mp: MathProcessor[N]): LatexString =
    (eqs map equationToLatex(varName1, varName2)) mkString ("\\begin{cases}", """\\""" + "\n", "\\end{cases}")
}
