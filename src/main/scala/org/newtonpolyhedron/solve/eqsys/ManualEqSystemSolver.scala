package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.ex.CancelledByUserException
import org.newtonpolyhedron.utils.LatexConversion._
import org.newtonpolyhedron.utils.parsing.ParseFormats._

/**
 * Allows user to solve system of equations manually.
 *
 * @author FS
 */
class ManualEqSystemSolver[N <: MPNumber](solverInput: EqSystemSolutionInput[N])(implicit mp: MathProcessor[N])
    extends EqSystemSolver[N] {

  override def whyCantSolve(system: Polys[N]): Option[String] = None

  override def solve(system: Polys[N]): Seq[NumVec[N]] = {
    def solveWithMessage(msgOption: Option[LatexString], valsOption: Option[Seq[String]]): Seq[NumVec[N]] = {
      solverInput.getInputFor(system, valsOption, msgOption) match {
        case Some(input) => proceedWithInput(input)
        case None        => throw CancelledByUserException
      }
    }
    def proceedWithInput(input: Seq[String]): Seq[NumVec[N]] = {
      val parsedInput = parseInput(input)
      if (parsedInput forall (_.isRight)) {
        val solution = parsedInput map (_.right.get)
        // Checking if the solution satisfies all polynomials
        system collectFirst {
          case poly if !poly.isZeroWithValues(solution) =>
            (poly, poly.totalWithValuesNonStrict(solution))
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

  private def parseInput(input: Seq[String]): Seq[Either[String, N]] = input map { s =>
    try {
      Right(mp.fromRational(parseFrac(s)))
    } catch {
      case e: Exception => Left(s"""Failed to parse "$s" as a fraction""")
    }
  }

  private def notSatisfiedMessage(poly: Polynomial[N], substitution: Seq[N]): LatexString = {
    val signedSubstitutionStrings = substitution map { s => (s.signum, s.abs.toLatexString) }
    latex(textToLatex("Solution does not satisfy ")
      + polynomialToLatex(solverInput.varName)(poly)
      + textToLatex(":")
      + "\\\\"
      + textToLatex("it evaluates to ")
      + signedAbsSeqToLatex(signedSubstitutionStrings))
  }
}
