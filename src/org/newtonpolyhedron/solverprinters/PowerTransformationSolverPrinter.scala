package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.changevars.ChangerOfVariables
import org.newtonpolyhedron.solve.power.PowerTransformationSolver

class PowerTransformationSolverPrinter(solver: PowerTransformationSolver,
                                       val varChanger: ChangerOfVariables,
                                       val poly1: Polynomial,
                                       val poly2: Polynomial,
                                       val p1idx1: Int,
                                       val p1idx2: Int,
                                       val p2idx1: Int,
                                       val p2idx2: Int,
                                       output: PrintWriter)
    extends SolverPrinter[PowerTransformationSolver](solver, output) {

  private def s = IndexedSeq

  private def polyToStr(poly: Polynomial): String = {
    poly map (term => "(" + term.coeff.fracValue + ") * " + term.powers.elements.zipWithIndex.map {
      case (pow, i) => s"x${i + 1}^($pow)"
    }.mkString(" * ")) mkString ("", " + ", " = 0")
  }

  override def solveFor(solver: PowerTransformationSolver,
                        output: PrintWriter) = {
    output.println(title("Solving equation system"))

    val (p1p1, p1p2, p2p1, p2p2) = (poly1(p1idx1), poly1(p1idx2), poly2(p2idx1), poly2(p2idx2))

    val alpha = solver.alphaFromTermSubtractionPairs(s((p1p2, p1p1), (p2p2, p2p1)))
    //    val alpha = matrFromVecs(s(s(-1, -1, 5), s(2, -1, 0), s(-1, 0, 2)) map vec.fromInts)
    val shortSubs1 = solver.substitute(s(p1p1, p1p2), alpha)
    output.println(s"Short substituted p1: $shortSubs1")
    val shortSubs2 = solver.substitute(s(p2p1, p2p2), alpha)
    output.println(s"Short substituted p2: $shortSubs2")
    val subs1 = solver.substitute(poly1, alpha)
    output.println(s"Substituted p1:       $subs1")
    val subs2 = solver.substitute(poly2, alpha)
    output.println(s"Substituted p2:       $subs2")
    output.println(polyToStr(subs1))
    output.println(polyToStr(subs2))

    val sol = solver.solveShortSubstitutesSystem(s(shortSubs1, shortSubs2))
    output.println(s"Solution:             $sol")
    val changeVarPolys = solver.varChangeFromShortSubsSolution(sol)
    output.println(s"Change of var poly:   $changeVarPolys")
    val changedPoly1 = varChanger.changeVars(subs1, changeVarPolys)
    output.println(s"p1 with changed vars: $changedPoly1")
    output.println(s"   size:              ${changedPoly1.size}")
    val changedPoly2 = varChanger.changeVars(subs2, changeVarPolys)
    output.println(s"p2 with changed vars: $changedPoly2")
    output.println(s"   size:              ${changedPoly2.size}")
  }

}