package org.newtonpolyhedron.entity.math

/**
 * Trait for internal matrix representation user by associated `MathProcessor`
 *
 * @author FS
 */
trait MPMatrix {
  def rowCount: Int
  def colCount: Int
  def isSquare: Boolean = rowCount == colCount
}
