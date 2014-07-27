package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.Polys
import org.newtonpolyhedron.conversion.latex._
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.math.Polynomials._
import org.newtonpolyhedron.utils.BigFracFormat
import org.newtonpolyhedron.ex.CancelledByUserException
import org.newtonpolyhedron.Polynomial

/**
 * Allows you to solve system of equations manually
 */
class ManualEqSystemSolver(solverInput: EqSystemSolutionInput) extends EqSystemSolver {

  def whyCantSolve(system: Polys): Option[String] = None

  def solve(system: Polys): Seq[FracMathVec] = {
    def solveWithMessage(msgOption: Option[LatexString], valsOption: Option[Seq[String]]): Seq[FracMathVec] = {
      solverInput.getInputFor(system, valsOption, msgOption) match {
        case Some(input) => proceedWithInput(input)
        case None        => throw CancelledByUserException
      }
    }
    def proceedWithInput(input: Seq[String]): Seq[FracMathVec] = {
      val parsedInput = parseInput(input)
      if (parsedInput forall (_.isRight)) {
        val solution = parsedInput map (_.right.get)
        val s = solution map Product.apply
        // Checking if the solution satisfies all polynomials 
        system collectFirst {
          case poly if !poly.isZeroWithValues(s) =>
            (poly, poly.totalWithValuesNonStrict(s))
        } match {
          case None                => Seq(new FracMathVec(solution))
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

  private lazy val frFmt = new BigFracFormat

  private def parseInput(input: Seq[String]): Seq[Either[String, BigFrac]] = input map { s =>
    try {
      Right(frFmt.parse(s).asInstanceOf[BigFrac])
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
      new Term(Product(1), FracMathVec(1, 2, 3)),
      new Term(tricky, FracMathVec(1, 2, 3)),
      new Term(Product(4), FracMathVec(0, 0, 0))
    ),
    IndexedSeq(
      new Term(Product(BigFrac(-1, 2)), FracMathVec(BigFrac(-1, 2), BigFrac.ZERO, BigFrac(-333, 667))),
      new Term(Product(-2), FracMathVec(0, 0, 3))
    )
  )

  val s = new ManualEqSystemSolver(i)

  println(s.solve(eqs))
}
