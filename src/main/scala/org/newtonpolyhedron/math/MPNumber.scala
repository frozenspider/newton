package org.newtonpolyhedron.math

/**
 * Marker trait for internal number representation user by associated `MathProcessor`
 *
 * @author FS
 */
trait MPNumber {
  def isValid: Boolean
}
