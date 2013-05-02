package org.newtonpolyhedron.solve.cone

import java.io.PrintWriter

import org.newtonpolyhedron.entity.vector.IntMathVec

class ConeSolverImpl extends ConeSolver {

  override def solve(ineqs: IndexedSeq[IntMathVec],
                     basis: IndexedSeq[IntMathVec],
                     dim: Int,
                     output: PrintWriter): IndexedSeq[IntMathVec] = {
    require(ineqs forall (_.dim == dim), "Inequation vector with incorrect dimension")
    require(basis forall (_.dim == dim), "Basis vector with incorrect dimension")
    val basis2 = if (!basis.isEmpty) basis else initialBasis(dim)
    val fundamentalSolution = solveInner(ineqs, basis2, dim, output)
    // val rank = MatrixUtils.getRank(MatrixUtils.fromIntVector(proxyList(ineqs)))
    // val result = withoutZeroProductSolutions(ineqs, rank)(basis)
    fundamentalSolution
  }

  private def withoutZeroProductSolutions(base: IndexedSeq[IntMathVec],
                                          rank: Int)(testing: IndexedSeq[IntMathVec]) =
    if (base.isEmpty || testing.isEmpty) testing
    else testing.filter(v => {
      val toZero = base count (_ *+ v == 0)
      toZero >= rank - 1
    })

  def solveInner(eqSys: IndexedSeq[IntMathVec],
                 basis: IndexedSeq[IntMathVec],
                 dim: Int,
                 output: PrintWriter): IndexedSeq[IntMathVec] = {
    val zeroPoint = IntMathVec.zero(dim)
    val eqSysWithZeroFirst = zeroPoint +: eqSys

    val (finalBasis, fundSol) = solveRec(IndexedSeq(eqSysWithZeroFirst(0)),
      eqSysWithZeroFirst.tail, basis, IndexedSeq.empty, dim, 1, output)

    if (!finalBasis.isEmpty) {
      assert(finalBasis.size == 1, "Unexpected basis left in the end: " + finalBasis)
      // If after all we still have single remaining basis vector, then
      // this is degenerate case (assumption, may be wrong!)
      // Since it's degenerate, both it it's negation are conforming
      (finalBasis ++ finalBasis.map(vec => -vec)).distinct
    } else {
      val fundSolWrapped = wrap(fundSol)

      val fundSolCleaned = for {
        sol <- fundSolWrapped
        validationRes = valudateSolution(sol, eqSysWithZeroFirst)
        if validationRes != NOT_CONFORMS
      } yield validationRes match {
        case CONFORMS_NEG => -sol
        case CONFORMS_POS => sol
        case NOT_CONFORMS => throw new RuntimeException("Should not be possible, please report")
      }

      val fundSolCleanedWrapped = wrap(fundSolCleaned)
      fundSolCleanedWrapped
    }
  }

  private def solveRec(eqSysPrev: IndexedSeq[IntMathVec],
                       eqSysRemn: IndexedSeq[IntMathVec],
                       basis: IndexedSeq[IntMathVec],
                       fundSol: IndexedSeq[IntMathVec],
                       dim: Int,
                       currIdx: Int,
                       output: PrintWriter): (IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) =
    if (eqSysRemn.isEmpty) (basis, fundSol)
    else {
      output.println("\n === Step " + currIdx + " ===")
      fundSolAndBasisOutput(basis, fundSol, output)
      val currEq = eqSysRemn.head
      val (basis2, fundSol2) =
        if (basis exists (currEq *+ _ != 0)) {
          solveEqSysWithBasis(currEq, basis, fundSol)
        } else {
          (basis, solveEqSysWithoutBasis(currEq, eqSysPrev, fundSol, dim))
        }
      solveRec(eqSysPrev :+ currEq, eqSysRemn.tail, basis2, fundSol2, dim, currIdx + 1, output)
    }

  private def initialBasis(dim: Int) =
    for (d <- 0 until dim) yield IntMathVec.zero(dim).updated(d, 1)

  /**
   * One iteration of an inequation system solving for ???
   *
   * @param currEq
   * @param tempBasis
   *            solution basis, cannot be empty
   * @param fundSol
   * @return new basis and fundamental solutions
   */
  private def solveEqSysWithBasis(currEq: IntMathVec,
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
    val nonInverted = basis map (v => (v *+ currEq, v))
    val (l, tempBasis) = (nonInverted map {
      case (l, v) => if (l <= 0) (l, v) else (-l, -v)
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
  private def solveEqSysWithoutBasis(currEq: IntMathVec,
                                     eqSysPart: IndexedSeq[IntMathVec],
                                     fundSol: IndexedSeq[IntMathVec],
                                     dim: Int): IndexedSeq[IntMathVec] = {
    def shouldCombine(v1: IntMathVec, v2: IntMathVec) = {
      // Our original "zero"-equations are the past equations giving zero for both v+ and v-
      val zeroEqs = eqSysPart filter (eq => (v1 *+ eq, v2 *+ eq) == (0, 0))
      val otherSols = fundSol filter (sol => sol != v1 && sol != v2)
      // Zero-equations L should contain non-zero for all other solutions
      otherSols forall (sol => zeroEqs exists (_ *+ sol != 0))
    }
    val (zrs, neg, pos) = {
      val (pos, rest) = fundSol partition (_ *+ currEq > 0)
      val (zrs, neg) = rest partition (_ *+ currEq == 0)
      (zrs, neg, pos)
    }
    val combined = for {
      n <- neg
      p <- pos
      if dim == 2 || fundSol.size == 2 || shouldCombine(n, p)
    } yield {
      val ln = n *+ currEq
      val lp = p *+ currEq

      val combinedSol = ((n * lp) - (p * ln)).reduced

      valudateSolution(combinedSol, eqSysPart) match {
        case CONFORMS_POS => combinedSol
        case CONFORMS_NEG => -combinedSol
        case NOT_CONFORMS => throw new RuntimeException("Somehow, non-conforming +/- combination")
      }
    }
    val newFundSol = zrs ++ neg ++ combined
    wrap(newFundSol)
  }

  def valudateSolution(cVec: IntMathVec,
                       solution: IndexedSeq[IntMathVec]): ValidationResult = {
    // We don't need to skip leading [0,0,0] vector
    if (solution forall (_ *+ cVec <= 0)) CONFORMS_POS
    else if (solution forall (_ *+ -cVec <= 0)) CONFORMS_NEG
    else NOT_CONFORMS
  }

  /** Removes duplicates and zero vectors. */
  private def wrap(vecList: IndexedSeq[IntMathVec]) =
    vecList.distinct filter (!_.isZero)

  private def fundSolAndBasisOutput(basis: IndexedSeq[IntMathVec],
                                    fundSol: IndexedSeq[IntMathVec],
                                    output: PrintWriter): Unit = {
    def printSeq[A](xs: Seq[A]): Unit =
      if (xs.isEmpty) output.println(" (none)")
      else xs foreach output.println
    output.println("=== Basis: ===")
    printSeq(basis)
    output.println("=== Fundamental Solution: ===")
    printSeq(fundSol)
  }

  sealed abstract trait ValidationResult
  case object CONFORMS_POS extends ValidationResult
  case object CONFORMS_NEG extends ValidationResult
  case object NOT_CONFORMS extends ValidationResult
}