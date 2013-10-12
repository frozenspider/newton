package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.changevars.ChangerOfVariables
import org.newtonpolyhedron.solve.power.PowerTransformationSolver

class PowerTransformationSolverPrinter(solver: PowerTransformationSolver,
                                       val varChanger: ChangerOfVariables,
                                       val polys: Seq[Polynomial],
                                       val indices: Seq[Seq[Int]],
                                       output: PrintWriter)
    extends SolverPrinter[PowerTransformationSolver](solver, output) {

  private def s = IndexedSeq

  private def polyToStr(poly: Polynomial): String = {
    poly map (term => "(" +
      term.coeff.fracValue +
      ") * " +
      term.powers.elements.mapWithIndex((pow, i) => s"x${i + 1}^($pow)").mkString(" * ")
    ) mkString ("", " + ", " = 0")
  }

  override def solveFor(solver: PowerTransformationSolver,
                        output: PrintWriter) = {
    output.println(title("Solving equation system"))

    val pts = (polys zip indices) map {
      case (poly, polyIndices) => polyIndices map poly
    }

    val (alpha, chosenPts) = solver.generateAlphaFromTerms(pts)
    //    val alpha = matrFromVecs(s(s(-1, -1, 5), s(2, -1, 0), s(-1, 0, 2)) map vec.fromInts)
    val shortSubsSeq = chosenPts map {
      case (p1, p2) => solver.substitute(s(p1, p2), alpha)
    }
    shortSubsSeq eachWithIndex { (shortSubs, i) => output.println(s"Short substituted p${i + 1}: $shortSubs") }
    val subsSeq = polys map (poly => solver.substitute(poly, alpha))
    subsSeq eachWithIndex { (subs, i) => output.println(s"Substituted p${i + 1}: $subs") }
    subsSeq eachWithIndex { (subs, i) => output.println(polyToStr(subs)) }

    val sol = solver.solveShortSubstitutesSystem(shortSubsSeq)
    output.println(s"Solution:             $sol")
    val changeVarPolys = solver.varChangeFromShortSubsSolution(sol)
    output.println(s"Change of var poly:   $changeVarPolys")
    val changedPolys = subsSeq mapWithIndex { (subs, i) =>
      val changedPoly = varChanger.changeVars(subs, changeVarPolys)
      output.println(s"p${i + 1} with changed vars: $changedPoly")
      output.println(s"   size:              ${changedPoly.size}")
      changedPoly
    }
  }

}