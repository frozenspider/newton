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
    val fundamentalSolution = solveInner(ineqs, basis, dim, output)
    //    val rank = MatrixUtils.getRank(MatrixUtils.fromIntVector(proxyList(ineqs)))
    //    val result = withoutZeroProductSolutions(ineqs, rank)(basis)
    fundamentalSolution
  }

  private def withoutZeroProductSolutions(base: IndexedSeq[IntMathVec],
                                          rank: Int)(testing: IndexedSeq[IntMathVec]) =
    if (base.isEmpty || testing.isEmpty) testing
    else testing.filter(vec => {
      val toZero = base count (_ *+ vec == 0)
      toZero >= rank - 1
    })

  def solveInner(eqSys: IndexedSeq[IntMathVec],
                 wishfulBasis: IndexedSeq[IntMathVec],
                 dim: Int,
                 output: PrintWriter): IndexedSeq[IntMathVec] = {
    val zeroPoint = IntMathVec.zero(dim)
    val eqSysWithZeroFirst = zeroPoint +: eqSys

    val basis =
      if (!wishfulBasis.isEmpty) wishfulBasis else initialBasis(dim)
    val (endBasis, fundSol) = solveRec(IndexedSeq(eqSysWithZeroFirst(0)),
      eqSysWithZeroFirst.tail, basis, IndexedSeq.empty, dim, 1, output)

    if (!endBasis.isEmpty) {
      assert(endBasis.size == 1, "Unexpected basis left in the end: " + endBasis)
      // If after all we still have single remaining basis vector, then
      // this is degenerate case (assumption, may be wrong!)
      // Since it's degenerate, both it it's negation are conforming
      (endBasis ++ (endBasis map (vec => -vec))).distinct
    } else {
      val fundSolWrapped = wrap(fundSol)

      val fundSolCleaned = for {
        sol <- fundSolWrapped
        validationRes = valudateSolution(sol, eqSysWithZeroFirst)
        if validationRes != NOT_CONFORMS
      } yield validationRes match {
        case CONFORMS_NEG => -sol
        case CONFORMS_POS => sol
        case NOT_CONFORMS => throw new RuntimeException("Not possible, just to please the compiler")
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
        if (nonZeroValsExists(basis, currEq)) {
          solveEqSysWithBasis(currEq, basis, fundSol)
        } else {
          (basis, solveEqSysWithoutBasis(currEq, eqSysPrev, fundSol, dim))
        }
      solveRec(eqSysPrev :+ currEq, eqSysRemn.tail, basis2, fundSol2, dim, currIdx + 1, output)
    }

  private def initialBasis(dim: Int) =
    for (currDim <- 0 until dim) yield IntMathVec.zero(dim).updated(currDim, 1)

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
  private def solveEqSysWithoutBasis(currEq: IntMathVec,
                                     eqSysPart: IndexedSeq[IntMathVec],
                                     fundSol: IndexedSeq[IntMathVec],
                                     dim: Int): IndexedSeq[IntMathVec] = {
    def unrollFundSol(fundSols: IndexedSeq[IntMathVec])(zrs: IndexedSeq[IntMathVec],
                                                        neg: IndexedSeq[IntMathVec],
                                                        pos: IndexedSeq[IntMathVec]): (IndexedSeq[IntMathVec], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) = {
      if (fundSols.isEmpty) (zrs, neg, pos)
      else {
        val h = fundSols.head
        val unroll = unrollFundSol(fundSols.tail)_
        ((h *+ currEq) compare 0) match {
          case 0  => unroll(zrs :+ h, neg, pos)
          case -1 => unroll(zrs, neg :+ h, pos)
          case 1  => unroll(zrs, neg, pos :+ h)
        }
      }
    }
    def shouldCombine(v1: IntMathVec, v2: IntMathVec) = {
      if (dim == 2 || fundSol.size == 2) true
      else {
        val zeroEqs = eqSysPart filter (selEq => (IndexedSeq(v1 *+ selEq, v2 *+ selEq) == IndexedSeq(0, 0)))
        val amtsOfZeros = for {
          sol <- fundSol if sol != v1 && sol != v2
        } yield zeroEqs count (_ *+ sol == 0)
        !amtsOfZeros.contains(zeroEqs.size)
      }
    }

    val (zrs, neg, pos) = unrollFundSol(fundSol)(IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
    val combined = for {
      n <- neg
      p <- pos
      if (shouldCombine(n, p))
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
  private def wrap(vecList: IndexedSeq[IntMathVec]) =
    vecList.distinct filter (!_.isZero)

  private def fundSolAndBasisOutput(basis: IndexedSeq[IntMathVec],
                                    fundSol: IndexedSeq[IntMathVec],
                                    output: PrintWriter): Unit = {
    def printSeq[A](xs: Seq[A]): Unit =
      if (xs.isEmpty) output.println(" (none)")
      else for (x <- xs) output.println(x)
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