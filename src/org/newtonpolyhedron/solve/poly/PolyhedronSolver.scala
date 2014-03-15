package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec

trait PolyhedronSolver {
  def solve(points: Seq[FracMathVec],
            commonLimits: Seq[IntMathVec],
            wishfulBasis: Seq[IntMathVec],
            output: PrintWriter): KeyTable[IntMathVec, Int, Boolean]
}