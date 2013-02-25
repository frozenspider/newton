package org.newtonpolyhedron.solve.cone

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.vector.IntVector
import java.io.PrintWriter
import org.newtonpolyhedron.utils.MatrixUtils
import java.math.BigInteger
import org.newtonpolyhedron.utils.ArithUtils

class ConeSolverImpl extends ConeSolver {

  def solve(
    inequations: java.util.List[IntVector],
    basis: java.util.List[IntVector],
    dim: Int,
    output: PrintWriter): java.util.List[IntVector] = {
    val fundamentalSolution = solveInner(inequations, basis, dim, output)
    val rank = MatrixUtils.getRank(MatrixUtils.fromIntVector(inequations))
    val result = removeZeroProductSolutions(inequations, fundamentalSolution, rank)
    return seq2list(fundamentalSolution)
  }

  private def removeZeroProductSolutions(
    base: IndexedSeq[IntVector],
    testing: IndexedSeq[IntVector],
    rank: Int): IndexedSeq[IntVector] = {
    if (base.isEmpty || testing.isEmpty) testing
    else {
      testing.filter(vec => {
        var toZero = 0;
        for (basePt <- base) {
          if (ArithUtils.isZero(basePt.dotProduct(vec))) {
            toZero += 1;
          }
        }
        toZero >= rank - 1
      })
    }
  }

  private def solveInner(
    eqSys: IndexedSeq[IntVector],
    wishfulBasis: IndexedSeq[IntVector],
    dim: Int,
    output: PrintWriter): IndexedSeq[IntVector] = {
    val zeroPoint = IntVector.empty(dim)
    val eqSysWithZeroFirst =
      if (eqSys(0) == zeroPoint) eqSys else (zeroPoint +: eqSys)

    val basis = getInitialBasis(dim, wishfulBasis)
    val fundSol = solveInitial(Vector(eqSysWithZeroFirst(0)),
      eqSysWithZeroFirst.tail, 1, basis, Vector.empty, dim, output)

    val fundSolWrapped = wrap(fundSol)

    val fundSolCleaned = for {
      sol <- fundSolWrapped
      validationRes = valudateSolution(sol, eqSysWithZeroFirst)
      if validationRes != NOT_CONFORMS
    } yield validationRes match {
      case CONFORMS_NEG => sol.negate
      case CONFORMS_POS => sol
    }

    val fundSolCleanedWrapped = wrap(fundSolCleaned)
    fundSolCleanedWrapped
  }

  private def solveInitial(
    eqSysPrev: IndexedSeq[IntVector],
    eqSysNext: IndexedSeq[IntVector],
    currIdx: Int,
    basis: IndexedSeq[IntVector],
    fundSol: IndexedSeq[IntVector],
    dim: Int,
    output: PrintWriter): IndexedSeq[IntVector] = {
    if (eqSysNext.isEmpty) fundSol
    else {
      val currEq = eqSysNext(0)
      val res = if (nonZeroValsExists(basis, currEq)) {
        val (basis2, fundSol2) = solveEqSystem_withBasis(currEq, basis, fundSol)
        solveInitial(eqSysPrev :+ currEq, eqSysNext.tail, currIdx + 1, basis2, fundSol2, dim, output)
      } else {
        val fundSol2 = solveEqSystem_withoutBasis(currEq, eqSysPrev, fundSol, dim)
        solveInitial(eqSysPrev :+ currEq, eqSysNext.tail, currIdx + 1, basis, fundSol2, dim, output)
      }
      output.println("\n === Step " + currIdx + " ===")
      fundSolAndBasisOutput(basis, fundSol, output)
      res
    }
  }

  private def getInitialBasis(dim: Int, wishfulBasis: IndexedSeq[IntVector]) = {
    if (wishfulBasis.isEmpty) {
      val basis = for (currDim <- 0 until dim) yield IntVector.empty(dim).withValueAt(currDim, BigInteger.ONE)
      basis
    } else {
      wishfulBasis
    }
  }

  private def nonZeroValsExists(basis: IndexedSeq[IntVector], eq: IntVector) = {
    basis.exists(vec => !ArithUtils.isZero(eq dotProduct vec))
  }

  /**
   * One iteration of an inequation system solving for ???
   *
   * @param currEq
   * @param tempBasis
   *            solution basis, cannot be empty
   * @param fundSol
   * @return new basis and fundamental solutions
   */
  private def solveEqSystem_withBasis(
    currEq: IntVector,
    basis: IndexedSeq[IntVector],
    fundSol: IndexedSeq[IntVector]): (IndexedSeq[IntVector], IndexedSeq[IntVector]) = {
    require(!basis.isEmpty, "Empty basis")

    val newFundSols = Vector.empty[IntVector]

    // Pre-calculate values of l(u) AND reverse vector of an old basis
    val (l, tempBasis) = calculateLAndTempBasis(currEq, basis);

    val currActiveIdx = findBasisWorkingElementIndex(l);

    val lActive = l.get(currActiveIdx);
    val basisActive = tempBasis(currActiveIdx);
    val basisActive2 = basis(currActiveIdx);

    // Add new basis line
    val newBasis = for (i <- 0 until tempBasis.size if i != currActiveIdx) yield {
      val basisCurr = tempBasis(i);
      val lCurr = l.get(i);

      // New basis element
      // basis[i] * l[cai] - basis[cai] * l[i]
      //@formatter:off
      val newBasisElement =
        basisCurr.multiply(lActive).subtract(basisActive /* Should be basisActive2 ? */ .multiply(lCurr)).getReduced();
      //@formatter:on

      newBasisElement
    }

    // Add new fundamental solution line
    val newFundSolsTemp = for (val fundSolCurr <- fundSol) yield {
      // l(v[n]) element from current fundSol
      val lVn = currEq.dotProduct(fundSolCurr);

      // New fundSol element
      // basis[cai] * lVn - fundSol[i] * l[cai];
      val newFundSol = basisActive.multiply(lVn).subtract(fundSolCurr.multiply(lActive)).getReduced();
      newFundSol;
    }

    val res = basisActive +: newFundSolsTemp

    return (newBasis, wrap(res))
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
  private def calculateLAndTempBasis(
    currEq: IntVector,
    basis: IndexedSeq[IntVector]): (IntVector, IndexedSeq[IntVector]) = {
    var found = false;
    val (lBase, tempBasis) = (for (basisVec <- basis) yield {
      val lBaseCurr = currEq.dotProduct(basisVec);
      val inverse = if (!found && !ArithUtils.isZero(lBaseCurr)) {
        found = true;
        ArithUtils.greater(lBaseCurr, BigInteger.ZERO);
      } else false
      if (inverse) {
        (lBaseCurr.negate, basisVec.negate)
      } else {
        (lBaseCurr, basisVec)
      }
    }).unzip

    // Non-reduced lBase
    (new IntVector(seq2list(lBase)), tempBasis)
  }

  /**
   * Returns an index of a working element index of l(u[i]) - the first non-zero.
   *
   * @param l
   *            l(u[i]) vector
   * @return working element index, or -1 if vector is zero-sized.
   */
  private def findBasisWorkingElementIndex(l: IntVector): Int = {
    val found = for {
      i <- 0 until l.getDim()
    } yield {
      val lCurr = l.get(i)
      if (ArithUtils.isZero(lCurr)) -1 else i
    }
    val res = found.find(_ != -1)
    res match {
      case Some(x) => x
      case None => throw new RuntimeException
    }
  }

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
  private def solveEqSystem_withoutBasis(
    currEq: IntVector,
    eqSysPart: IndexedSeq[IntVector],
    fundSol: IndexedSeq[IntVector],
    dim: Int): IndexedSeq[IntVector] = {

    val fundSolZero = new java.util.ArrayList[IntVector];
    val fundSolMinus = new java.util.ArrayList[IntVector];
    val fundSolPlus = new java.util.ArrayList[IntVector];
    val fundSolCombined = new java.util.ArrayList[IntVector];

    for (currFundSol <- fundSol) {
      val dotProd = currEq.dotProduct(currFundSol);

      if (ArithUtils.isZero(dotProd)) {
        fundSolZero.add(currFundSol);
      } else if (ArithUtils.less(dotProd, BigInteger.ZERO)) {
        fundSolMinus.add(currFundSol);
      } else {
        fundSolPlus.add(currFundSol);
      }
    }

    for {
      cNegative <- fundSolMinus
      cPositive <- fundSolPlus
    } {
      val shouldCombine = if (dim == 2 || fundSol.size == 2) true
      else {
        val zeroEqs = new java.util.ArrayList[IntVector];
        val amtsOfZeros = new java.util.ArrayList[Integer];

        for (selEq <- eqSysPart) {
          if (ArithUtils.areZeros(selEq.dotProduct(cPositive), selEq.dotProduct(cNegative))) {
            zeroEqs.add(selEq);
          }
        }
        zeroEqs.remove(0); // Skip leading [0,0,0] vector (is it necessary?)

        for (currFundSol <- fundSol) {
          var zeros = 0;
          for (zeroEq <- zeroEqs) {
            if (true //
              && !currFundSol.equals(cPositive)
              && !currFundSol.equals(cNegative)
              && ArithUtils.isZero(zeroEq.dotProduct(currFundSol))) {
              zeros += 1;
            }
          }
          amtsOfZeros.add(zeros);
        }
        !amtsOfZeros.contains(zeroEqs.size);
      }
      if (shouldCombine) {
        val lNegative = currEq.dotProduct(cNegative);
        val lPositive = currEq.dotProduct(cPositive);

        val combinedSol = cNegative.multiply(lPositive).subtract(cPositive.multiply(lNegative)).getReduced();

        valudateSolution(combinedSol, eqSysPart) match {
          case CONFORMS_POS =>
            fundSolCombined.add(combinedSol)
          case CONFORMS_NEG =>
            fundSolCombined.add(combinedSol.negate())
          case NOT_CONFORMS => // Impossible
            throw new RuntimeException("Somehow, non-conforming +/- combination");
        }
      }
    }
    val newFundSol = fundSolZero ++ fundSolMinus ++ fundSolCombined
    return wrap(newFundSol);
  }

  def valudateSolution(cVec: IntVector, eqSysPart: IndexedSeq[IntVector]): ValidationResult = {
    // We don't need to skip leading [0,0,0] vector
    var conform = true;
    for (eq <- eqSysPart) {
      if (ArithUtils.greater(eq.dotProduct(cVec), BigInteger.ZERO)) {
        conform = false;
        // break;
      }
    }
    if (conform) return CONFORMS_POS;

    //
    // Negative check
    //
    conform = true;
    val cVecNeg = cVec.negate
    for (eq <- eqSysPart) {
      if (ArithUtils.greater(eq.dotProduct(cVecNeg), BigInteger.ZERO)) {
        conform = false;
        // break;
      }
    }
    if (conform) CONFORMS_NEG
    else NOT_CONFORMS;
  }

  /**
   * Removes duplicates and zero vectors. Doesn't cause side effects.
   *
   * @param vecList
   *            input
   * @return refined input
   */
  protected def wrap(vecList: IndexedSeq[IntVector]): IndexedSeq[IntVector] = {
    if (vecList.isEmpty) vecList;
    else {
      val dim = vecList(0).getDim();

      // Removing duplicates
      val vectorSet = Set(vecList: _*);
      // Removing zero vectors
      (vectorSet - IntVector.empty(dim)).toIndexedSeq
    }
  }

  private def fundSolAndBasisOutput(
    basis: IndexedSeq[IntVector],
    fundSol: IndexedSeq[IntVector],
    output: PrintWriter): Unit = {
    output.println("=== Basis: ===");
    if (basis.size > 0) {
      for (currBasis <- basis) {
        output.println(currBasis);
      }
    } else {
      output.println(" (none)");
    }
    output.println("=== Fundamental Solution: ===");
    for (currSol <- fundSol) {
      output.println(currSol);
    }
  }

  abstract trait ValidationResult
  case object CONFORMS_POS extends ValidationResult
  case object CONFORMS_NEG extends ValidationResult
  case object NOT_CONFORMS extends ValidationResult
}