package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.NewtonImports._

trait PolyhedronSolver[N <: MPNumber] {
  def solve(
      points:             Seq[NumVec[N]],
      commonLimitsOption: Option[Seq[IntVec]],
      wishfulBasisOption: Option[Seq[IntVec]],
      output:             PrintWriter
  ): KeyTable[IntVec, Int, Boolean]
}
