package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter
import java.util.Comparator

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntVector
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.utils.NullPrintWriter
import org.newtonpolyhedron.utils.PointUtils

class PolyMotzkinBurgerSolver(val coneSolver: ConeSolver) extends PolyhedronSolver {

  override def solve(points: java.util.List[FractionVector],
                     commonLimits: java.util.List[IntVector],
                     wishfulBasis: java.util.List[IntVector],
                     output: PrintWriter) = {
    val tmp = solve(points map (x => fracvec2mathvec(x)),
      commonLimits map (x => intvec2mathvec(x)),
      wishfulBasis map (x => intvec2mathvec(x)),
      output)
    val converted = new ArrayListKeyTable[IntVector, Integer, java.lang.Boolean]
    for {
      rk <- tmp.rowKeyList
      ck <- tmp.colKeyList
    } converted.put(mathvec2intvec(rk), ck, if (tmp.get(rk, ck)) true else null)
    converted
  }

  override def solve(points: IndexedSeq[FracMathVec],
                     commonLimits: IndexedSeq[IntMathVec],
                     wishfulBasis: IndexedSeq[IntMathVec],
                     output: PrintWriter): KeyTable[IntMathVec, Int, Boolean] = {
    val allSolutions = solveForEachPoint(points, commonLimits, wishfulBasis, output)
    fillTableWith(allSolutions)
  }

  def solveForEachPoint(points: IndexedSeq[FracMathVec],
                        commonLimits: IndexedSeq[IntMathVec],
                        wishfulBasis: IndexedSeq[IntMathVec],
                        output: PrintWriter): IndexedSeq[IndexedSeq[IntMathVec]] = {
    val dim = points.head.dim
    val allSolutions = for (currPtIdx <- 0 until points.size) yield {
      // Forming equations by substracting current point, plus common limits - if any
      val eqSys = PointUtils.copySubtractPointAsInt(points, currPtIdx) ++ commonLimits
      val coneSolutions = coneSolver.solve(eqSys, wishfulBasis, dim, NullPrintWriter.instance)
      coneSolutions
    }
    allSolutions
  }

  val intComparator: Comparator[Int] = new Comparator[Int] {
    override def compare(o1: Int, o2: Int) = o1 compare o2
  }

  def fillTableWith(allSolutions: IndexedSeq[IndexedSeq[IntMathVec]]): KeyTable[IntMathVec, Int, Boolean] = {
    val lookupTable = new ArrayListKeyTable[IntMathVec, Int, Boolean]
    fillTableIdxKeys(lookupTable, allSolutions.size)
    for {
      i <- 0 until allSolutions.size
      val coneSols = allSolutions(i)
      sol <- coneSols
    } lookupTable.put(sol, i, true)
    KeyTables.sortByRowHeaders(lookupTable, true)
    KeyTables.sortByColHeaders(lookupTable, intComparator, true)
    lookupTable
  }

  def fillTableIdxKeys(lookupTable: KeyTable[IntMathVec, Int, Boolean],
                       upTo: Int) = {
    for (i <- 0 until upTo)
      lookupTable.put(null, i, false)
    lookupTable.removeRow(null)
  }
}