package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter
import java.util.Comparator

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.utils.PointUtils

class PolyMotzkinBurgerSolver[N <: MPNumber](val coneSolver: ConeSolver)(implicit mp: MathProcessor[N])
    extends PolyhedronSolver[N] {

  override def solve(
      points:             Seq[NumVec[N]],
      commonLimitsOption: Option[Seq[IntVec]],
      wishfulBasisOption: Option[Seq[IntVec]],
      output:             PrintWriter
  ): KeyTable[IntVec, Int, Boolean] = {
    val allSolutions = solveForEachPoint(points, commonLimitsOption, wishfulBasisOption, output)
    fillTableWith(allSolutions)
  }

  def solveForEachPoint(
      points:             Seq[NumVec[N]],
      commonLimitsOption: Option[Seq[IntVec]],
      wishfulBasisOption: Option[Seq[IntVec]],
      output:             PrintWriter
  ): Seq[Seq[IntVec]] = {
    val dim = points.head.size
    val allSolutions = for (currPtIdx <- 0 until points.size) yield {
      val commonLimits = commonLimitsOption getOrElse IndexedSeq.empty
      // Forming equations by substracting current point, plus common limits - if any
      val eqSys = PointUtils.copySubtractPointAsInt(points, currPtIdx) ++ commonLimits
      val coneSolutions = coneSolver.solve(eqSys, wishfulBasisOption, dim)
      coneSolutions
    }
    allSolutions
  }

  val intComparator: Comparator[Int] = new Comparator[Int] {
    override def compare(o1: Int, o2: Int) = o1 compare o2
  }

  def fillTableWith(allSolutions: Seq[Seq[IntVec]]): KeyTable[IntVec, Int, Boolean] = {
    // Looks way cleaner with variable than with values and folds
    var lookupTable = KeyTable.empty[IntVec, Int, Boolean]
    for (i <- 0 until allSolutions.size)
      lookupTable = lookupTable.withEmptyCol(i)
    for {
      (coneSols, i) <- allSolutions.zipWithIndex
      sol <- coneSols
    } lookupTable = lookupTable + (sol, i, true)
    lookupTable = lookupTable.sortedCols
    lookupTable = lookupTable.sortedRows
    lookupTable
  }
}
