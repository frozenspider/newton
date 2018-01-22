package org.newtonpolyhedron.entity.math

/**
 * Mathematical processor subcomponent which performs operations on matrices.
 *
 * @author FS
 */
trait MatrixMathProcessor[N <: MPNumber] {

  //
  // Processing
  //

  def add(m1: Matrix[N], m2: Matrix[N]): Matrix[N]

  def subtract(m1: Matrix[N], m2: Matrix[N]): Matrix[N]

  def multiply(m1: Matrix[N], m2: Matrix[N]): Matrix[N]

  def negate(m: Matrix[N]): Matrix[N]

  // TODO: Use pseudo-inverse?
  /** Inverse */
  def inverse(m: Matrix[N]): Matrix[N]

  def minor(m: Matrix[N], skipRow: Int, skipCol: Int): N

  def minorMatrix(m: Matrix[N], skipRow: Int, skipCol: Int): Matrix[N]

  def det(m: Matrix[N]): N

  def rank(m: Matrix[N]): Int

  def map[A, B](m: Matrix[A], f: A => B): Matrix[B]

  def exists(m: Matrix[N], cond: N => Boolean): Boolean = m.elementsByRow exists (e => cond(e._3))

  def forall(m: Matrix[N], cond: N => Boolean): Boolean = m.elementsByRow forall (e => cond(e._3))

  def contains(m: Matrix[N], what: N): Boolean = m.elementsByRow exists (_._3 == what)

  //
  // Advanced operations
  //

  /**
   * Converges the matrix to a triangle form, where all elements below main diagonal are zeros.
   *
   * This operation doesn't change determinant value, but it may change it's sign.
   *
   * Example of a triangle matrix (empty cells = zeros):
   *
   * {{{
   * +--+--+--+--+
   * | 1| 2| 3| 4|
   * +--+--+--+--+
   * |  | 2| 3| 4|
   * +--+--+--+--+
   * |  |  | 3| 4|
   * +--+--+--+--+
   * |  |  |  | 4|
   * +--+--+--+--+
   * }}}
   *
   * @return matrix and `1` or `-1` depending on whether or not determinant sign was reversed
   */
  def triangleForm(m: Matrix[N]): (Matrix[N], Int)

  /**
   * Converts the matrix to diagonal form.
   *
   * Returns diagonal matrix alongside with row and column transformation matrices.
   *
   * Needed for unimodular matrices maker via Euler (or whatever is its name, not sure yet) algorithm.
   *
   * @return diagonal matrix alongside with row and column transformation matrices
   * in form (`DiagonalMatrix`, `RowTransformationsMatrix`, `ColumnTransformationsMatrix`)
   */
  def diagonalize(m: Matrix[N]): (Matrix[N], Matrix[N], Matrix[N])
}
