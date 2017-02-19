package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.VectorImports._

trait PolyhedronSolver {
  def solve(points: Seq[FracVec],
            commonLimitsOption: Option[Seq[IntVec]],
            wishfulBasisOption: Option[Seq[IntVec]],
            output: PrintWriter): KeyTable[IntVec, Int, Boolean]
}
