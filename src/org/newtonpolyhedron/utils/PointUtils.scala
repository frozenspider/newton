package org.newtonpolyhedron.utils

import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec

import javax.vecmath.Point3d

object PointUtils {

  def p3d(x: Double, y: Double, z: Double) = new Point3d(x, y, z)

  def p3d(v: FracMathVec): Point3d =
    new Point3d(v(0).toDouble,
      if (v.dim > 1) v(1).toDouble else 0,
      if (v.dim > 2) v(2).toDouble else 0)

  /**
   * Creates a vector list, each element of which is produced by subtracting point with given
   * index from the rest (and excluding it).
   *
   * @param points
   *            source list.
   * @param indexToSubtract
   *            index of point to subtract and exclude.
   * @return vector list of size {@code n-1}, with {@code i}'th point subtracted from all other.
   */
  def copySubtractPointAsInt(points: IndexedSeq[FracMathVec],
                             idxToSubs: Int): IndexedSeq[IntMathVec] = {
    require(points.size > idxToSubs, "Index to big for a collection: " + idxToSubs + " in " + points)
    val toSub = points(idxToSubs)
    val result = points map (pt => IntMathVec.fromFrac(pt - toSub)) filter (!_.isZero)
    result
  }
}