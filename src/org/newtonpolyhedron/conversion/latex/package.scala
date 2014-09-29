package org.newtonpolyhedron.conversion

import org.newtonpolyhedron.entity._
import org.newtonpolyhedron.entity.equation._
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PolynomialUtils._

package object latex {

  sealed trait LatexStringMixin

  type LatexString = String with LatexStringMixin

  def latex(s: String) = stringToLatex(s)

  private implicit def stringToLatex(s: String) = s.asInstanceOf[LatexString]

  def fracToLatex(frac: BigFrac): LatexString = {
    if (frac.isInt) {
      frac.num.toString
    } else {
      val fracVal = "\\frac{" + (frac.num.abs) + "}{" + frac.den + "}"
      (if (frac.signum < 0) "-" else "") + fracVal
    }
  }

  def productToLatex(product: Product): LatexString =
    if (product.isRational) {
      fracToLatex(product.fracValue)
    } else {
      // Irrational case
      val (rational, roots) = product.rootedForm
      val rationalString = if (rational == 1) "" else rational.toString
      val irrationalString = roots.toSeq sortBy (_._1) map {
        case (_, rooted) if rooted == 0 => ""
        case (rootBase, rooted)         => s"\\sqrt[$rootBase]{$rooted}"
      }
      "(" + rationalString + irrationalString.mkString + ")"
    }

  def signToLatex(sign: EquationSign): LatexString = sign match {
    case EquationSign.Equals    => "="
    case EquationSign.Greater   => ">"
    case EquationSign.GreaterEq => "\\leq"
    case EquationSign.Less      => "<"
    case EquationSign.LessEq    => "\\geq"
  }

  private def powersToLatex(varName: String)(pows: Seq[BigFrac]): LatexString = {
    val opts = pows mapWithIndex { (power, i) =>
      if (power.isZero)
        None
      else
        Some(
          variableToLatex(varName, Some(i + 1)) + "^{" + fracToLatex(power) + "}"
        )
    }
    opts.yieldDefined.mkString
  }

  def variableToLatex(varName: String, index: Option[Int]): LatexString =
    index match {
      case None    => varName
      case Some(i) => varName + "_{" + i + "}"
    }

  def polynomialToLatex(varName: String)(poly: Polynomial): LatexString = {
    if (poly.isEmpty) {
      ""
    } else {
      val termsSignedStrings = poly map { term =>
        (
          term.coeff.signum,
          productToLatex(term.coeff * term.coeff.signum) + powersToLatex(varName)(term.powers)
        )
      }
      val (headCoeff, headTermAbsString) = termsSignedStrings.head
      val restTermsString = termsSignedStrings.tail.foldLeft("") {
        case (start, (coeff, termAbsString)) if coeff < 0 => start + "-" + termAbsString
        case (start, (coeff, termAbsString))              => start + "+" + termAbsString
      }
      val termsString = headCoeff match {
        case x if x < 0 => "-" + headTermAbsString + restTermsString
        case x          => headTermAbsString + restTermsString
      }
      termsString
    }
  }

  def equationToLatex(varName1: String, varName2: String)(eq: Equation): LatexString = {
    val lhs = polynomialToLatex(varName1)(eq.lhs)
    val rhs = polynomialToLatex(varName2)(eq.rhs)
    val sign = signToLatex(eq.sign)
    lhs + sign + rhs
  }

  def equationsToLatex(eqs: Equations, varName1: String, varName2: String): LatexString =
    (eqs map equationToLatex(varName1, varName2)) mkString ("\\begin{cases}", """\\""" + "\n", "\\end{cases}")
}
