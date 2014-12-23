package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter
import java.util.Comparator

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.utils.NullPrintWriter
import org.newtonpolyhedron.utils.PointUtils

class PolyMotzkinBurgerSolver(val coneSolver: ConeSolver) extends PolyhedronSolver {

  override def solve(points: Seq[FracVec],
                     commonLimitsOption: Option[Seq[IntVec]],
                     wishfulBasisOption: Option[Seq[IntVec]],
                     output: PrintWriter): KeyTable[IntVec, Int, Boolean] = {
    val allSolutions = solveForEachPoint(points, commonLimitsOption, wishfulBasisOption, output)
    fillTableWith(allSolutions)
  }

  def solveForEachPoint(points: Seq[FracVec],
                        commonLimitsOption: Option[Seq[IntVec]],
                        wishfulBasisOption: Option[Seq[IntVec]],
                        output: PrintWriter): Seq[Seq[IntVec]] = {
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
    val lookupTable = new ArrayListKeyTable[IntVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, allSolutions.size)
    for {
      (coneSols, i) <- allSolutions.zipWithIndex
      sol <- coneSols
    } lookupTable.put(sol, i, true)
    KeyTables.sortByRowHeaders(lookupTable, intVecOrdering, true)
    KeyTables.sortByColHeaders(lookupTable, intComparator, true)
    lookupTable
  }

  def fillTableIdxKeys(lookupTable: KeyTable[IntVec, Int, Boolean],
                       upTo: Int): Unit = {
    for (i <- 0 until upTo)
      lookupTable.put(null, i, false)
    lookupTable.removeRow(null)
  }
}
