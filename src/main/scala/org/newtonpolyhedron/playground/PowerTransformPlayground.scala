package org.newtonpolyhedron.solve.power

import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.NewtonLogic
import org.newtonpolyhedron.math.internal.InternalMathProcessor
import org.newtonpolyhedron.solve.changevars.ChangerOfVariablesImpl
import org.newtonpolyhedron.solve.cone.MotzkinBurger
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMakerImpl
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolverImpl
import org.newtonpolyhedron.solverprinters.PolyIntersectionSolverPrinter

import spire.math.Rational

object PowerTransformPlayground {
  private type N = org.newtonpolyhedron.math.internal.Product
  private implicit val mp: MathProcessor[N] = new InternalMathProcessor
  private type Vec = NumVec[N]
  private type Powers = IndexedSeq[Vec]

  def s = IndexedSeq
  def vec = NumVec

  private def veci(s: Seq[Int]) = vec(s map mp.fromInt: _*)
  private def vecbi(s: Seq[BigInt]) = vec(s map mp.fromBigInt: _*)
  private def vecf(s: Seq[Rational]) = vec(s map mp.fromRational: _*)
  private def polyToStr(poly: Polynomial[N]): String = {
    poly map (term => "(" +
      term.coeff.toRational +
      ") * " +
      term.powers.mapWithIndex((pow, i) => s"x${i + 1}^($pow)").mkString(" * ")) mkString ("", " + ", " = 0")
  }

  private def polyFromPts(components: (Int, Seq[Int])*): Polynomial[N] =
    components map { case (coeff, pows) => Term(mp.fromInt(coeff), veci(pows)) } toIndexedSeq

  //
  // SANDBOX
  //
  def getVarIdx(t: Term[N]) = t.powers indexWhere (_ != mp.zero)

  def countNonZeroPowers(t: Term[N]) = t.powers count (_ != mp.zero)

  def banishIntoO(p: Polynomial[N]): Polynomial[N] = {
    // TODO: Do we need `i` here?
    // Removing multiplications
    val p1: Polynomial[N] = p filter (t => countNonZeroPowers(t) <= 1)
    // Removing terms higher powers if terms with lower powers are present
    val p2: Polynomial[N] = (p1 groupBy (getVarIdx)).toIndexedSeq sortBy (_._1) map {
      case (varIdx, terms) =>
        terms.sortBy(_.powers(varIdx)).head
    }
    p2
  }

  def extractVar(banishedPoly: Polynomial[N], i: Int): (Term[N], Polynomial[N]) = {
    assert(banishedPoly.forall(countNonZeroPowers(_) <= 1), "Banishment into O(n) should be performed at this point")
    val (preLhs, preRhs) = banishedPoly.partition(t => getVarIdx(t) == i)
    assert(preLhs.size == 1, s"Cannot extract ${i + 1}'th variable from given poly (after banishment): $banishedPoly")
    val lhs = preLhs.head
    val rhs: Polynomial[N] = preRhs map (t => -t) sortBy (getVarIdx)
    val res = (lhs / lhs.coeff, rhs map (_ / lhs.coeff))
    res
  }

  //
  //
  //
  def main(args: Array[String]): Unit = {
    val powTransfSolver = new PowerTransformationSolverImpl(
      new UnimodularMatrixMakerImpl,
      (new NewtonLogic).systemOfEqSolverChain
    )
    val polyhedronIntersSolver = new PolyIntersectionSolverImpl(new MotzkinBurger)
    val output = new PrintWriter(System.out, true)
    //
    // Definition
    //
    //    val poly1 = polyFromPts(
    //      (+1, s(3, 4, 5)),
    //      (+1, s(5, 5, 8)),
    //      (+1, s(2, 1, 1)),
    //      (+1, s(0, 0, 3)))
    //    val poly2 = polyFromPts(
    //      (-1, s(7, 6, 11)),
    //      (-1, s(9, 7, 14)),
    //      (-4, s(1, 1, 1)),
    //      (-1, s(5, 0, 0)))
    val poly1 = polyFromPts(
      (+40, s(1, 1, 1)),
      (+25, s(4, 0, 0)),
      (-25, s(0, 4, 0)),
      (-1, s(0, 0, 4)),
      (+16, s(2, 0, 2))
    )
    val poly2 = polyFromPts(
      (-1, s(0, 1, 1)),
      (-1, s(2, 0, 1)),
      (-1, s(3, 1, 0)),
      (-1, s(2, 1, 1))
    )
    val polysWithIndices = s(
      (poly1, s(0, 1, 3, 4)),
      (poly2, s(0, 1))
    )
    val dim = 3
    val polys = polysWithIndices map (_._1)
    val intersectionPtsIndices = polysWithIndices map (_._2)

    //
    // Transformation/substitution
    //
    println("Solving equation system")
    val shotPolys: Polys[N] = polysWithIndices map {
      case (poly, polyIndices) => polyIndices.toIndexedSeq map poly
    }
    val shotPolys2: Polys[N] =
      (polys zip intersectionPtsIndices).toIndexedSeq map {
        case (poly, polyIndices) => polyIndices.toIndexedSeq map poly
      }
    assert(shotPolys == shotPolys2)
    //    val alpha = powTransfSolver.generateAlphaFromTerms(shotPolys map (_.powers))
    // TODO: Fix implicit to avoid direct map call
    val alpha: Matrix[N] = mp.matrix.map(Matrix[Int](s(s(-3, 1, 1), s(2, -1, 0), s(-1, 1, 0))), (x => mp.fromInt(x)))
    println("Alpha:")
    println(alpha)
    println("Alpha-1:")
    println(alpha.inverse)
    def substituteAllIn(polynomials: Polys[N]) = {
      polynomials map (powTransfSolver.substitute(_, alpha))
    }
    val shortSubs = substituteAllIn(shotPolys)
    shortSubs foreachWithIndex { (shortSubs, i) =>
      output.println(s"Short substituted p${i + 1}: $shortSubs")
    }
    val fullSubs = substituteAllIn(polys)
    fullSubs foreachWithIndex { (subs, i) =>
      output.println(s"Full substituted p${i + 1}:  $subs")
    }

    val shortSubsSolution = vecf(s(-1, -1, 0)) // powTransfSolver.solveShortSubstitutesSystem(shortSubs)
    output.println(s"Solution:             $shortSubsSolution")
    val changeVarPolys = powTransfSolver.varChangeFromShortSubsSolution(shortSubsSolution)
    output.println(s"Change of var poly:   $changeVarPolys")

    // After performing change of vars
    val changedPolys = fullSubs mapWithIndex { (subs, i) =>
      val changed = (new ChangerOfVariablesImpl).changeVars(subs, changeVarPolys)
      output.println(s"p${i + 1} with changed vars (size: ${changed.size}):")
      output.println(s"  $changed")
      changed
    }
    // After performing O(n) banishent
    val banishedPolys = changedPolys mapWithIndex { (changed, i) =>
      val banished = banishIntoO(changed)
      output.println(s"p${i + 1} banished into O(n):")
      output.println(s"  $banished")
      banished
    }
    // After performing variable extraction
    val extractedPolys = banishedPolys mapWithIndex { (banished, i) =>
      val (extractedTerm, extractedRhs) = extractVar(banished, i)
      output.println(s"p${i + 1} extracted:")
      output.println(s"  ${extractedTerm} = ${extractedRhs}")
      banished
    }

    //
    // Polynomials => Polyhedrons, intersecting them
    //
    //    val polyhedrons = extractedPolys map (_ map (_.powers))
    val polyhedrons = polys map (_ map (_.powers))
    val polyhedronIntersSolverPrinter = new PolyIntersectionSolverPrinter(polyhedronIntersSolver, polyhedrons, dim, output)
    polyhedronIntersSolverPrinter.solveAndPrint
  }
}
