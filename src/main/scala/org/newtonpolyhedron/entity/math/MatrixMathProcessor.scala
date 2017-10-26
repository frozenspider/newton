package org.newtonpolyhedron.entity.math

trait MatrixMathProcessor[N <: MPNumber, M <: MPMatrix] {
  // Construction

  def apply(elements: Iterable[Iterable[N]]): M
  def idenitiy(dim: Int): M
  def zero(dim: Int): M
  def zero(rowCount: Int, colCount: Int): M
  def empty: M

  // Processing

  def get(m: M, row: Int, col: Int): N

  def add(m1: M, m2: M): M

  def subtract(m1: M, m2: M): M

  def multiply(m1: M, m2: M): M

  def negate(m: M): M

  // TODO: Use pseudo-inverse?
  /** Inverse */
  def inverse(m: M): M

  def transpose(m: M): M

  def minor(m: M, skipRow: Int, skipCol: Int): N

  def minorMatrix(m: M, skipRow: Int, skipCol: Int): M

  def det(m: M): N

  def rank(m: M): Int

  def map(m: M, f: N => N): M

  def exists(m: M, cond: N => Boolean): Boolean = elementsByRow(m) exists (e => cond(e._3))

  def forall(m: M, cond: N => Boolean): Boolean = elementsByRow(m) forall (e => cond(e._3))

  def contains(m: M, what: N): Boolean = elementsByRow(m) exists (_._3 == what)

  /** @return stream of (row, col, element) */
  def elementsByRow(m: M): Stream[(Int, Int, N)]

  def rows(m: M): IndexedSeq[IndexedSeq[N]]

  def cols(m: M): IndexedSeq[IndexedSeq[N]]

  def addRow(m: M, row: Traversable[N]): M

  def addCol(m: M, col: Traversable[N]): M

  //
  // Advanced operations
  //

  /**
   * Converges the matrix to a triangle form, where all elements below main diagonal are zeros.
   * <p>
   * This operation doesn't change determinant value, but it may change it's sign.
   * <p>
   * Example of a triangle matrix (empty cells = zeros):
   *
   * <pre>
   * +--+--+--+--+
   * | 1| 2| 3| 4|
   * +--+--+--+--+
   * |  | 2| 3| 4|
   * +--+--+--+--+
   * |  |  | 3| 4|
   * +--+--+--+--+
   * |  |  |  | 4|
   * +--+--+--+--+
   * </pre>
   *
   * @return matrix and {@code 1} or {@code -1} depending on whether or not determinant sign was reversed
   */
  def triangleForm(m: M): (M, Int)

  /**
   * Converts the matrix to diagonal form.
   * <p>
   * Returns diagonal matrix alongside with row and column transformation matrices
   * @return (`DiagonalMatrix`, `RowTransformationsMatrix`, `ColumnTransformationsMatrix`)
   */
  def diagonalize(m: M): (M, M, M)
}
