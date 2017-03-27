package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.ex.CancelledByUserException
import org.newtonpolyhedron.utils.LatexConversion._
import org.newtonpolyhedron.utils.parsing.ParseFormats._

import spire.math.Rational

/**
 * Allows you to solve system of equations manually
 */
class ManualEqSystemSolver[N <: MPNumber](solverInput: EqSystemSolutionInput[N])(implicit mp: MathProcessor[N])
    extends EqSystemSolver[N] {

  def whyCantSolve(system: Polys[N]): Option[String] = None

  def solve(system: Polys[N]): Seq[FracVec] = {
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
        val s = solution map mp.fromRational
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

  private def notSatisfiedMessage(poly: Polynomial[N], substitution: Seq[N]): LatexString = {
    val signedSubstitutionStrings = substitution map { s => (s.signum, productToLatex(s.abs)) }
    latex(textToLatex("Solution does not satisfy ")
      + polynomialToLatex(solverInput.varName)(poly)
      + textToLatex(":")
      + "\\\\"
      + textToLatex("it evaluates to ")
      + signedAbsSeqToLatex(signedSubstitutionStrings))
  }
}
