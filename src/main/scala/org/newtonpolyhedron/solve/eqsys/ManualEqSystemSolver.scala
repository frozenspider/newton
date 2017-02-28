package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.ex.CancelledByUserException
import org.newtonpolyhedron.utils.LatexConversion._
import org.newtonpolyhedron.utils.PolynomialUtils._
import org.newtonpolyhedron.utils.parsing.ParseFormats._

import spire.math.Rational

/**
 * Allows you to solve system of equations manually
 */
class ManualEqSystemSolver(solverInput: EqSystemSolutionInput) extends EqSystemSolver {

  def whyCantSolve(system: Polys): Option[String] = None

  def solve(system: Polys): Seq[FracVec] = {
    def solveWithMessage(msgOption: Option[LatexString], valsOption: Option[Seq[String]]): Seq[FracVec] = {
      solverInput.getInputFor(system, valsOption, msgOption) match {
        case Some(input) => proceedWithInput(input)
        case None        => throw CancelledByUserException
      }
    }
    def proceedWithInput(input: Seq[String]): Seq[FracVec] = {
      val parsedInput = parseInput(input)
      if (parsedInput forall (_.isRight)) {
        val solution = parsedInput map (_.right.get)
        val s = solution map Product.apply
        // Checking if the solution satisfies all polynomials
        system collectFirst {
          case poly if !poly.isZeroWithValues(s) =>
            (poly, poly.totalWithValuesNonStrict(s))
        } match {
          case None                => Seq(solution.toIndexedSeq)
          case Some((poly, subst)) => solveWithMessage(Some(notSatisfiedMessage(poly, subst)), Some(input))
        }
      } else {
        solveWithMessage(parsedInput.collectFirst {
          case Left(msg) => textToLatex(msg)
        }, Some(input))
      }
    }
    solveWithMessage(None, None)
  }

  private def parseInput(input: Seq[String]): Seq[Either[String, Rational]] = input map { s =>
    try {
      Right(parseFrac(s))
    } catch {
      case e: Exception => Left(s"""Failed to parse "$s" as a fraction""")
    }
  }

  private def notSatisfiedMessage(poly: Polynomial, substitution: Seq[Product]): LatexString = {
    val signedSubstitutionStrings = substitution map { s => (s.signum, productToLatex(s.abs)) }
    latex(textToLatex("Solution does not satisfy ")
      + polynomialToLatex(solverInput.varName)(poly)
      + textToLatex(":")
      + "\\\\"
      + textToLatex("it evaluates to ")
      + signedAbsSeqToLatex(signedSubstitutionStrings))
  }
}

object ManualEqSystemSolver extends App {
  import org.newtonpolyhedron._
  import org.newtonpolyhedron.entity._
  import org.newtonpolyhedron.entity.vector._
  import org.newtonpolyhedron.entity.equation._

  val i = new org.newtonpolyhedron.ui.eqsys.EqSystemSolutionDialogInput

  val tricky = org.newtonpolyhedron.ui.eqsys.EqSystemRenderingPanel.tricky

  val eqs: Polys = IndexedSeq[Polynomial](
    IndexedSeq(
      new Term(Product(1), FracVec(1, 2, 3)),
      new Term(tricky, FracVec(1, 2, 3)),
      new Term(Product(4), FracVec(0, 0, 0))
    ),
    IndexedSeq(
      new Term(Product(Rational(-1, 2)), FracVec(Rational(-1, 2), Rational.zero, Rational(-333, 667))),
      new Term(Product(-2), FracVec(0, 0, 3))
    )
  )

  val s = new ManualEqSystemSolver(i)

  println(s.solve(eqs))
}
