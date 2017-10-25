package org.newtonpolyhedron.entity.math

/**
 * @author FS
 */
class MPMatrix(
  val rowCount: Int,
  val colCount: Int
) {
  def isSquare: Boolean = rowCount == colCount
}
