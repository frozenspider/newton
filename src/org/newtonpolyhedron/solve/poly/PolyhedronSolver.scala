package org.newtonpolyhedron.solve.poly
import java.io.PrintWriter

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntVector

trait PolyhedronSolver {
  def solve(points: java.util.List[FractionVector],
            commonLimits: java.util.List[IntVector],
            wishfulBasis: java.util.List[IntVector],
            output: PrintWriter): KeyTable[IntVector, Integer, java.lang.Boolean]

  def solve(points: IndexedSeq[FracMathVec],
            commonLimits: IndexedSeq[IntMathVec],
            wishfulBasis: IndexedSeq[IntMathVec],
            output: PrintWriter): KeyTable[IntMathVec, Int, Boolean]
}