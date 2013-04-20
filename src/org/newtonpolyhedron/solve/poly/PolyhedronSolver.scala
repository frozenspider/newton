package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec

trait PolyhedronSolver {
  def solve(points: IndexedSeq[FracMathVec],
            commonLimits: IndexedSeq[IntMathVec],
            wishfulBasis: IndexedSeq[IntMathVec],
            output: PrintWriter): KeyTable[IntMathVec, Int, Boolean]
}