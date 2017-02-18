package org.newtonpolyhedron.utils

import org.newtonpolyhedron.entity.vector.VectorImports._

import javax.vecmath.Point3d

object PointUtils {

  def p3d(x: Double, y: Double, z: Double): Point3d = new Point3d(x, y, z)
  def p3d(coord: (Double, Double, Double)): Point3d = p3d(coord._1, coord._2, coord._3)

  def p3d(v: FracVec): Point3d = p3d(
    v(0).toDouble,
    if (v.size > 1) v(1).toDouble else 0,
    if (v.size > 2) v(2).toDouble else 0
  )

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
  def copySubtractPointAsInt(points: Seq[FracVec],
                             idxToSubs: Int): Seq[IntVec] = {
    require(points.size > idxToSubs, "Index to big for a collection: " + idxToSubs + " in " + points)
    val toSub = points(idxToSubs)
    val result = for {
      pt <- points
      sub = IntVec.fromFracVec(pt - toSub)
      if !sub.isZero
    } yield sub
    result
  }
}
