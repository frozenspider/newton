package org.newtonpolyhedron.solve.cone

import java.io.PrintWriter
import java.math.BigInteger
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.vector.IntVector
import org.newtonpolyhedron.entity.vector.MathVector
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.utils.MatrixUtils

class ConeSolverImpl extends ConeSolver {

  def proxyVec(vec: java.util.List[IntVector]): IndexedSeq[IntMathVec] = vec map (x => intvec2mathvec(x))
  def proxyVec(vec: IndexedSeq[IntVector]): IndexedSeq[IntMathVec] = vec map (x => intvec2mathvec(x))

  def solve(inequations: java.util.List[IntVector],
            basis: java.util.List[IntVector],
            dim: Int,
            output: PrintWriter): java.util.List[IntVector] = {

    val ineqs2 = proxyVec(inequations)
    val basis2 = proxyVec(basis)

    val fundamentalSolution = solveInner(ineqs2, basis2, dim, output)
    val rank = MatrixUtils.getRank(MatrixUtils.fromIntVector(inequations))
    val result = removeZeroProductSolutions(ineqs2, basis2, rank)
    return seq2list(fundamentalSolution map (x => mathvec2intvec(x)))
  }

  private def removeZeroProductSolutions(base: IndexedSeq[IntMathVec],
                                         testing: IndexedSeq[IntMathVec],
                                         rank: Int): IndexedSeq[IntMathVec] = {
    if (base.isEmpty || testing.isEmpty) testing
    else testing.filter(vec => {
      val toZero = base count (_ *+ vec == 0)
      toZero >= rank - 1
    })
  }

  private def solveInner(eqSys: IndexedSeq[IntMathVec],
                         wishfulBasis: IndexedSeq[IntMathVec],
                         dim: Int,
                         output: PrintWriter): IndexedSeq[IntMathVec] = {
    val zeroPoint = IntMathVec.zero(dim)
    val eqSysWithZeroFirst =
      if (eqSys(0) == zeroPoint) eqSys else (zeroPoint +: eqSys)

    val basis = getInitialBasis(dim, wishfulBasis)
    val fundSol = solveInitial(Vector(eqSysWithZeroFirst(0)),
      eqSysWithZeroFirst.tail, basis, Vector.empty, dim, 1, output)

    val fundSolWrapped = wrap(fundSol)

    val fundSolCleaned = for {
      sol <- fundSolWrapped
      validationRes = valudateSolution(sol, eqSysWithZeroFirst)
      if validationRes != NOT_CONFORMS
    } yield validationRes match {
      case CONFORMS_NEG => -sol
      case CONFORMS_POS => sol
    }

    val fundSolCleanedWrapped = wrap(fundSolCleaned)
    fundSolCleanedWrapped
  }

  private def solveInitial(eqSysPrev: IndexedSeq[IntMathVec],
                           eqSysNext: IndexedSeq[IntMathVec],
                           basis: IndexedSeq[IntMathVec],
                           fundSol: IndexedSeq[IntMathVec],
                           dim: Int,
                           currIdx: Int,
                           output: PrintWriter): IndexedSeq[IntMathVec] = {
    if (eqSysNext.isEmpty) fundSol
    else {
      val currEq = eqSysNext(0)
      val res = if (nonZeroValsExists(basis, currEq)) {
        val (basis2, fundSol2) = solveEqSystemWithBasis(currEq, basis, fundSol)
        solveInitial(eqSysPrev :+ currEq, eqSysNext.tail, basis2, fundSol2, dim, currIdx + 1, output)
      } else {
        val fundSol2 = solveEqSystemWithoutBasis(currEq, eqSysPrev, fundSol, dim)
        solveInitial(eqSysPrev :+ currEq, eqSysNext.tail, basis, fundSol2, dim, currIdx + 1, output)
      }
      output.println("\n === Step " + currIdx + " ===")
      fundSolAndBasisOutput(basis, fundSol, output)
      res
    }
  }

  private def getInitialBasis(dim: Int, wishfulBasis: IndexedSeq[IntMathVec]) =
    if (wishfulBasis.isEmpty) {
      val basis = for (currDim <- 0 until dim) yield IntMathVec.zero(dim).updated(currDim, 1)
      basis
    } else {
      wishfulBasis
    }

  private def nonZeroValsExists(basis: IndexedSeq[IntMathVec], eq: IntMathVec) =
    basis.exists(vec => eq *+ vec != 0)

  /**
   * One iteration of an inequation system solving for ???
   *
   * @param currEq
   * @param tempBasis
   *            solution basis, cannot be empty
   * @param fundSol
   * @return new basis and fundamental solutions
   */
  private def solveEqSystemWithBasis(currEq: IntMathVec,
                                      basis: IndexedSeq[IntMathVec],
                                      fundSol: IndexedSeq[IntMathVec]): (IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) = {
    require(!basis.isEmpty, "Empty basis")

    // Pre-calculate values of l(u) and reverse vector of an old basis
    val (l, tempBasis) = calculateLAndTempBasis(currEq, basis)

    val currActiveIdx = findBasisWorkingElementIndex(l)

    val lActive = l(currActiveIdx)
    val basisActive = tempBasis(currActiveIdx)
    val basisActive2 = basis(currActiveIdx)

    // Add new basis line
    val newBasis = for {
      i <- 0 until tempBasis.size if i != currActiveIdx
      val basisCurr = tempBasis(i)
      val lCurr = l(i)
      /* Should we use basisActive2 ? */
    } yield ((basisCurr * lActive) - (basisActive * lCurr)).reduced

    // Add new fundamental solution line
    val newFundSolsTemp = fundSol map (curr => {
      val lVn = currEq *+ curr // l(v[n]) element from current fundSol
      ((basisActive * lVn) - (curr * lActive)).reduced
    })

    val newFundSols = wrap(basisActive +: newFundSolsTemp)
    (newBasis, newFundSols)
  }

  /**
   * Calculating l(u[i]). If necessary, inverses first fitting basis vector.
   *
   * @param currEq
   *            selected (in)equation
   * @param basis
   *            current basis
   * @return l(u[i]) vector and modified basis to be used with it
   */
  private def calculateLAndTempBasis(currEq: IntMathVec,
                                     basis: IndexedSeq[IntMathVec]): (IntMathVec, IndexedSeq[IntMathVec]) = {
    val nonInverted = basis.map(vec => (vec *+ currEq, vec))
    val (l, tempBasis) = (nonInverted map {
      case (l, vec) => if (l <= 0) (l, vec) else (-l, -vec)
    }).unzip

    // Non-reduced lBase
    (new IntMathVec(l), tempBasis)
  }

  /**
   * Returns an index of a working element index of l(u[i]) - the first non-zero.
   *
   * @param l
   *            l(u[i]) vector
   * @return working element index, or -1 if vector is zero-sized.
   */
  private def findBasisWorkingElementIndex(l: IntMathVec): Int =
    l.elements indexWhere (_ != 0)

  /**
   * Solves an equation system if basis is exhausted.
   *
   * @param currEq
   *            current equation
   * @param eqSysPart
   *            part of equation system, from first to current equation exclusive (btw, why part?)
   * @param fundSol
   *            fundamental solution
   * @param dim
   *            space dimension
   * @return new fundamental solution
   */
  private def solveEqSystemWithoutBasis(currEq: IntMathVec,
                                         eqSysPart: IndexedSeq[IntMathVec],
                                         fundSol: IndexedSeq[IntMathVec],
                                         dim: Int): IndexedSeq[IntMathVec] = {
    def unrollFundSol(fundSols: IndexedSeq[IntMathVec])(zrs: IndexedSeq[IntMathVec],
                                                        neg: IndexedSeq[IntMathVec],
                                                        pos: IndexedSeq[IntMathVec]): (IndexedSeq[IntMathVec], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) = {
      if (fundSols.isEmpty) (zrs, neg, pos)
      else {
        val currSol = fundSols.head
        val partial = unrollFundSol(fundSols.tail)_
        ((currSol *+ currEq) compare 0) match {
          case 0  => partial(zrs :+ currSol, neg, pos)
          case -1 => partial(zrs, neg :+ currSol, pos)
          case 1  => partial(zrs, neg, pos :+ currSol)
        }
      }
    }
    def shouldCombine(currNeg: IntMathVec,
                      currPos: IntMathVec) = {
      if (dim == 2 || fundSol.size == 2) true
      else {
        val zeroEqs = eqSysPart filter (selEq => (Vector(selEq *+ currPos, selEq *+ currNeg) == Vector(0, 0)))
        val amtsOfZeros = for {
          currFundSol <- fundSol
          if currFundSol != currPos && currFundSol != currNeg
        } yield zeroEqs count (eq => (eq *+ currFundSol) == 0)
        !amtsOfZeros.contains(zeroEqs.size)
      }
    }

    val (zrs, neg, pos) = unrollFundSol(fundSol)(Vector.empty, Vector.empty, Vector.empty)
    val combined = for {
      currNeg <- neg
      currPos <- pos
      if (shouldCombine(currNeg, currPos))
    } yield {
      val lNegative = currEq *+ currNeg
      val lPositive = currEq *+ currPos

      val combinedSol = ((currNeg * lPositive) - (currPos * lNegative)).reduced

      valudateSolution(combinedSol, eqSysPart) match {
        case CONFORMS_POS => combinedSol
        case CONFORMS_NEG => -combinedSol
        case NOT_CONFORMS => throw new RuntimeException("Somehow, non-conforming +/- combination")
      }
    }
    val newFundSol = zrs ++ neg ++ combined
    return wrap(newFundSol)
  }

  def valudateSolution(cVec: IntMathVec,
                       solution: IndexedSeq[IntMathVec]): ValidationResult = {
    // We don't need to skip leading [0,0,0] vector
    if (solution.find(_ *+ cVec > 0).isEmpty) CONFORMS_POS
    else if (solution.find(_ *+ -cVec > 0).isEmpty) CONFORMS_NEG
    else NOT_CONFORMS
  }

  /**
   * Removes duplicates and zero vectors. Doesn't cause side effects.
   *
   * @param vecList
   *            input
   * @return refined input
   */
  protected def wrap(vecList: IndexedSeq[IntMathVec]): IndexedSeq[IntMathVec] = {
    if (vecList.isEmpty) vecList
    else {
      val dim = vecList(0).dim

      // Removing duplicates
      val vectorSet = Set(vecList: _*)
      // Removing zero vectors
      (vectorSet - IntMathVec.zero(dim)).toIndexedSeq
    }
  }

  private def fundSolAndBasisOutput(basis: IndexedSeq[IntMathVec],
                                    fundSol: IndexedSeq[IntMathVec],
                                    output: PrintWriter): Unit = {
    output.println("=== Basis: ===")
    if (basis.size > 0) {
      for (currBasis <- basis) {
        output.println(currBasis)
      }
    } else {
      output.println(" (none)")
    }
    output.println("=== Fundamental Solution: ===")
    for (currSol <- fundSol) {
      output.println(currSol)
    }
  }

  abstract trait ValidationResult
  case object CONFORMS_POS extends ValidationResult
  case object CONFORMS_NEG extends ValidationResult
  case object NOT_CONFORMS extends ValidationResult
}