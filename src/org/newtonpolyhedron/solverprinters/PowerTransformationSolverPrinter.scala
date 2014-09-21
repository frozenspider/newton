package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.Polys
import org.newtonpolyhedron.SuperIterable
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.solve.changevars.ChangerOfVariables
import org.newtonpolyhedron.solve.power.PowerTransformationSolver

class PowerTransformationSolverPrinter(solver: PowerTransformationSolver,
                                       val varChanger: ChangerOfVariables,
                                       val polys: Polys,
                                       val intersectionPtsIndices: Seq[Seq[Int]],
                                       output: PrintWriter)
    extends SolverPrinter[PowerTransformationSolver](solver, output) {

  private def s = IndexedSeq

  override def solveFor(solver: PowerTransformationSolver,
                        output: PrintWriter) = {
    output.println(title("Solving equation system"))

    val shotPolys: Polys =
      (polys zip intersectionPtsIndices).toIndexedSeq map {
        case (poly, polyIndices) => polyIndices.toIndexedSeq map poly
      }
    val shortPolysPowers: Seq[Seq[FracMathVec]] =
      shotPolys map (_ map (_.powers))

    val alpha = solver.generateAlphaFromTerms(shortPolysPowers)
    //  val alpha = matrFromVecs(s(s(-1, -1, 5), s(2, -1, 0), s(-1, 0, 2)) map vec.fromInts)
    def substituteAllIn(polynomials: Polys) = polynomials map (solver.substitute(_, alpha))

    val shortSubs = substituteAllIn(shotPolys)
    shortSubs eachWithIndex { (shortSubs, i) =>
      output.println(s"Short substituted p${i + 1}: $shortSubs")
    }

    val fullSubs = substituteAllIn(polys)
    fullSubs eachWithIndex { (subs, i) =>
      output.println(s"Full substituted p${i + 1}:  $subs")
    }

    val shortSubsSolution = solver.solveShortSubstitutesSystem(shortSubs)
    output.println(s"Solution:             $shortSubsSolution")
    val changeVarPolys = solver.varChangeFromShortSubsSolution(shortSubsSolution)
    output.println(s"Change of var poly:   $changeVarPolys")

    val changeds = fullSubs mapWithIndex { (subs, i) =>
      val changed = varChanger.changeVars(subs, changeVarPolys)
      output.println(s"p${i + 1} with changed vars: $changed")
      output.println(s"   size: ${changed.size}")
      changed
    }
  }

}