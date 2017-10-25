package org.newtonpolyhedron.entity.math

import spire.math.Rational

/**
 * Mathematical processor capable of performing math operations
 * on some internal representation of numbers and numerical expressions.
 *
 * @author FS
 */
trait MathProcessor[N <: MPNumber, M <: MPMatrix] {
  def zero: N

  def one: N

  def isZero(x: N): Boolean

  /** Whether the number is a valid integer (of arbitrary size) */
  def isIntegral(x: N): Boolean

  /** Whether the number is a valid m/n fraction */
  def isRational(x: N): Boolean

  def compare(x: N, y: N): Int

  /** @return 0 for 0, otherwise -1 or 1 indicating on the leading sign */
  def signum(x: N): Int

  def abs(x: N): N

  def negate(x: N): N

  def inverse(x: N): N

  def add(x: N, y: N): N

  def subtract(x: N, y: N): N

  def multiply(x: N, y: N): N

  def divide(x: N, y: N): N

  /** Raise a number to a specified power */
  def raise(x: N, y: N): N

  /** Extract the principal root of the given number */
  def proot(x: N, y: N): N

  def fromDouble(x: Double): N

  def fromInt(x: Int): N

  def fromBigInt(x: BigInt): N

  def fromRational(x: Rational): N

  def toInt(x: N): Int

  def toLong(x: N): Long

  def toDouble(x: N): Double

  def toRational(x: N): Rational

  def toLatexString(x: N): String

  //
  // Matrix operations
  //

  def matrix: MatrixProcessor

  trait MatrixProcessor {
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

    def map[B: Numeric](m: M, f: N => B): M

    def exists(m: M, cond: N => Boolean): Boolean = elementsByRow(m) exists (e => cond(e._3))

    def forall(m: M, cond: N => Boolean): Boolean = elementsByRow(m) forall (e => cond(e._3))

    def contains(m: M, what: N): Boolean = elementsByRow(m) exists (_._3 == what)

    /** @return stream of (row, col, element) */
    def elementsByRow(m: M): Stream[(Int, Int, N)]

    def rows(m: M): IndexedSeq[IndexedSeq[N]]

    def cols(m: M): IndexedSeq[IndexedSeq[N]]

    def addRow(m: M, col: Traversable[N]): M

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
}
