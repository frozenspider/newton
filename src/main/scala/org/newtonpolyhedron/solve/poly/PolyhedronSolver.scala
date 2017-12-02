package org.newtonpolyhedron.solve.poly

import java.io.PrintWriter

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.NewtonImports._

trait PolyhedronSolver[N <: MPNumber] {
  /**
   * Given a number of points, compute a minimal convex hull polyhedron
   * along with normal vectors to each of its highest-level faces.
   *
   * Do so by solving a system of equations formed by subtracting point coordinates
   * from all other points - repeated for every point.
   *
   * @param points
   *            source points to compute convex hull around
   * @param commonLimitsOption
   *            common equations, added to system of equations for each point
   * @param wishfulBasisOption
   *            a basis for solving systems of equations
   * @return lookup table `KeyTable[NormalVectors, PointIndex, Correspondence]`
   */
  def solve(
      points:             Seq[NumVec[N]],
      commonLimitsOption: Option[Seq[IntVec]],
      wishfulBasisOption: Option[Seq[IntVec]] // TODO: Is it really used?
  ): KeyTable[IntVec, Int, Boolean]
}
