package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter

import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.VectorImports._

trait PolyhedronSolver {
  def solve(points: Seq[FracVec],
            commonLimits: Seq[IntVec],
            wishfulBasis: Seq[IntVec],
            output: PrintWriter): KeyTable[IntVec, Int, Boolean]
}
