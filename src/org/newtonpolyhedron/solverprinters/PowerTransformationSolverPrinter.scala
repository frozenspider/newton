package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter

import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.changevars.ChangerOfVariables
import org.newtonpolyhedron.solve.power.PowerTransformationSolver
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PolynomialUtils._

class PowerTransformationSolverPrinter(solver: PowerTransformationSolver,
                                       val varChanger: ChangerOfVariables,
                                       val polys: Polys,
                                       val intersectionPtsIndices: Seq[Seq[Int]],
                                       output: PrintWriter)
    extends SolverPrinter[PowerTransformationSolver](solver, output) {

  private def s = IndexedSeq

  override def solveFor(solver: PowerTransformationSolver,
                        output: PrintWriter) = {
    output.println(title("Power Transformation"))
    output.println(header("Initial polynomials"))
    polys foreach output.println

    output.println(header("Solving equation system"))
    val shotPolys: Polys =
      (polys zip intersectionPtsIndices).toIndexedSeq map {
        case (poly, polyIndices) => polyIndices.toIndexedSeq map poly
      }
    val shortPolysPowers: Seq[Seq[FracVec]] =
      shotPolys map (_ map (_.powers))

    val alpha = solver.generateAlphaFromTerms(shortPolysPowers)
    output.println(header("Alpha:"))
    output.println(alpha)
    //val alpha = matrFromVecs(s(s(-3, 1, 1), s(2, -1, 0), s(-1, 1, 0)) map vec.fromInts)
    //val alpha = Matrix.fromVectors[BigFrac](s(s(1, 1, 1), s(0, 1, 2), s(0, 0, 1)))

    def substituteAllIn(polynomials: Polys) =
      polynomials map (solver.substitute(_, alpha))

    val shortSubs = substituteAllIn(shotPolys)
    output.println(header("Short substituted polys"))
    shortSubs foreach output.println

    val fullSubs = substituteAllIn(polys)
    output.println(header("Full substituted polys"))
    fullSubs foreach output.println

    val shortSubsSolution: FracVec =
      solver.solveShortSubstitutesSystem(shortSubs)
    output.println(header("Solution of short substituted"))
    output.println(shortSubsSolution)

    val changeVarPolys = solver.varChangeFromShortSubsSolution(shortSubsSolution)
    output.println(header("Change of var poly"))
    output.println(changeVarPolys)

    // Performing change of vars
    output.println(header("Changing variables"))
    val changeds = fullSubs mapWithIndex { (subs, i) =>
      val changed = varChanger.changeVars(subs, changeVarPolys)
      output.println(changed)
      output.println(s"   (of size ${changed.size})")
      changed
    }

    // Performing Big-O banishent
    output.println(header("Banishing into Big-O"))
    val banisheds = changeds mapWithIndex { (changed, i) =>
      val banished = banishIntoBigO(changed)
      output.println(banished)
      banished
    }

    // Performing variable extraction
    output.println(header("Extracting variables:"))
    val extracteds = banisheds mapWithIndex { (banished, i) =>
      val (extractedTerm, extractedRhs) = extractVar(banished, i)
      output.println(s"${extractedTerm} = ${extractedRhs}")
      banished
    }
  }

  //
  // TODO: Extract logic
  //
  private def getVarIdx(t: Term) = t.powers indexWhere (_ != 0)

  private def countNonZeroPowers(t: Term) = t.powers count (_ != 0)

  private def banishIntoBigO(p: Polynomial): Polynomial = {
    // Removing multiplications
    val p1: Polynomial = p filter (t => countNonZeroPowers(t) <= 1)
    // Removing terms higher powers if terms with lower powers are present
    val p2: Polynomial = (p1 groupBy (getVarIdx)).toIndexedSeq sortBy (_._1) map {
      case (varIdx, terms) =>
        terms.sortBy(_.powers(varIdx)).head
    }
    p2
  }

  private def extractVar(banishedPoly: Polynomial, idx: Int): (Term, Polynomial) = {
    assert(banishedPoly.forall(countNonZeroPowers(_) <= 1), "Banishment into O(n) should be performed at this point")
    val (preLhs, preRhs) = banishedPoly.partition(t => getVarIdx(t) == idx)
    assert(preLhs.size == 1, s"Cannot extract ${idx + 1}'th variable from given poly (after banishment): $banishedPoly")
    val lhs = preLhs.head
    val rhs: Polynomial = preRhs map (t => -t) sortBy (getVarIdx)
    val res = (lhs / lhs.coeff, rhs map (_ / lhs.coeff))
    res
  }

}
