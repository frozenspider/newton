package org.newtonpolyhedron.math

/**
 * Use `import spire.compat._` to provide implicit converstion to Scala `Ordering` and `Numeric`
 *
 * @author FS
 */
trait MathImports {
  type MPNumber = org.newtonpolyhedron.entity.math.MPNumber
  type MPMatrix = org.newtonpolyhedron.entity.math.MPMatrix
  type MathProcessor[N <: MPNumber, M <: MPMatrix] = org.newtonpolyhedron.entity.math.MathProcessor[N, M]

  type Rational = spire.math.Rational
  val Rational = spire.math.Rational

  implicit class RichMPNumber[A, N <: MPNumber](x: A)(implicit mp: MathProcessor[N, _], conv: A => N) extends Ordered[N] {
    def isZero: Boolean = mp.isZero(x)
    def isIntegral: Boolean = mp.isIntegral(x)
    def isRational: Boolean = mp.isRational(x)

    override def compare(y: N): Int = mp.compare(x, y)

    def signum: Int = mp.signum(x)
    def abs: N = mp.abs(x)
    def unary_- : N = mp.negate(x)
    def inverse: N = mp.inverse(x)
    def +(y: N): N = mp.add(x, y)
    def -(y: N): N = mp.subtract(x, y)
    def *(y: N): N = mp.multiply(x, y)
    def /(y: N): N = mp.divide(x, y)

    def **(y: BigInt): N = mp.raise(x, mp.fromBigInt(y))
    def **(y: Rational): N = mp.raise(x, mp.fromRational(y))
    def **(y: N): N = mp.raise(x, y)

    def proot(y: BigInt): N = mp.proot(x, mp.fromBigInt(y))
    def proot(y: Rational): N = mp.proot(x, mp.fromRational(y))
    def proot(y: N): N = mp.proot(x, y)

    def intValue: Int = toInt
    def longValue: Long = toLong
    def doubleValue: Double = toDouble
    def rationalValue: Rational = toRational

    def toInt: Int = mp.toInt(x)
    def toLong: Long = mp.toLong(x)
    def toDouble: Double = mp.toDouble(x)
    def toRational: Rational = mp.toRational(x)
    def toLatexString: String = mp.toLatexString(x)
  }

  implicit class RichMPMatrix[M <: MPMatrix, N <: MPNumber](m: M)(implicit mp: MathProcessor[N, M]) {
    def apply(row: Int, col: Int): N = mp.matrix.get(m, row, col)

    def +(that: M): M = mp.matrix.add(m, that)
    def -(that: M): M = mp.matrix.subtract(m, that)
    def *(that: M): M = mp.matrix.multiply(m, that)
    def negate: M = mp.matrix.negate(m)

    def inverse: M = mp.matrix.inverse(m)
    def transpose: M = mp.matrix.transpose(m)

    def minor(skipRow: Int, skipCol: Int): N = mp.matrix.minor(m, skipRow, skipCol)
    def minorMatrix(skipRow: Int, skipCol: Int): M = mp.matrix.minorMatrix(m, skipRow, skipCol)

    def det: N = mp.matrix.det(m)
    def rank: Int = mp.matrix.rank(m)

    def map[B: Numeric](f: N => B) = mp.matrix.map(m, f)

    def exists(cond: N => Boolean): Boolean = mp.matrix.exists(m, cond)
    def forall(cond: N => Boolean): Boolean = mp.matrix.forall(m, cond)
    def contains(what: N): Boolean = mp.matrix.contains(m, what)

    /** @return stream of (row, col, element) */
    def elementsByRow: Stream[(Int, Int, N)] = mp.matrix.elementsByRow(m)

    def rows: IndexedSeq[IndexedSeq[N]] = mp.matrix.rows(m)
    def cols: IndexedSeq[IndexedSeq[N]] = mp.matrix.cols(m)

    def addRow(row: Traversable[N]) = mp.matrix.addRow(m, row)
    def addCol(col: Traversable[N]) = mp.matrix.addCol(m, col)

    def triangleForm: (M, Int) = mp.matrix.triangleForm(m)
    def diagonalize: (M, M, M) = mp.matrix.diagonalize(m)
  }

  implicit def mpNumberSupport[N <: MPNumber](implicit mp: MathProcessor[N, _]): MPNumberSupport[N] =
    new MPNumberSupport

  /**
   * Integration with Spire.
   *
   * Note that most conversion methods force any non-rational Spire number to be converted to Double.
   */
  class MPNumberSupport[N <: MPNumber](implicit mp: MathProcessor[N, _])
      extends spire.math.Numeric[N]
      with spire.algebra.CRing[N]
      with spire.algebra.Order[N] {

    import spire.math.Algebraic
    import spire.math.ConvertableFrom
    import spire.math.ConvertableTo
    import spire.math.Number
    import spire.math.Real

    override def zero: N = mp.zero
    override def one: N = mp.one
    override def compare(a: N, b: N) = new RichMPNumber(a) compare b
    override def plus(x: N, y: N): N = new RichMPNumber(x) + y
    override def minus(x: N, y: N): N = new RichMPNumber(x) - y
    override def times(x: N, y: N): N = new RichMPNumber(x) * y
    override def div(x: N, y: N): N = new RichMPNumber(x) / y
    override def negate(x: N): N = -new RichMPNumber(x)
    override def fromInt(x: Int): N = mp.fromInt(x)
    override def toInt(x: N): Int = new RichMPNumber(x).intValue
    override def toLong(x: N): Long = new RichMPNumber(x).longValue
    override def toFloat(x: N): Float = toDouble(x).floatValue
    override def toDouble(x: N): Double = new RichMPNumber(x).doubleValue

    // Members declared in spire.math.ConvertableFrom
    override def toAlgebraic(x: N): spire.math.Algebraic = toRational(x).toAlgebraic
    override def toBigDecimal(x: N): BigDecimal = ConvertableFrom.ConvertableFromRational.toBigDecimal(toRational(x))
    override def toBigInt(x: N): BigInt = toRational(x).toBigInt
    override def toByte(x: N): Byte = toInt(x).toByte
    override def toNumber(x: N): Number = Number(toRational(x))
    override def toRational(x: N): Rational = new RichMPNumber(x).toRational
    override def toShort(x: N): Short = toInt(x).toShort
    override def toString(x: N): String = x.toString
    override def toType[B](x: N)(implicit e: ConvertableTo[B]): B = e.fromRational(toRational(x))

    // Members declared in spire.math.ConvertableTo
    override def fromAlgebraic(x: Algebraic): N = if (x.isRational) fromRational(x.toRational.get) else fromDouble(x.toDouble)
    override def fromBigDecimal(x: BigDecimal): N = mp.fromRational(Rational(x))
    override def fromByte(x: Byte): N = fromInt(x)
    override def fromShort(x: Short): N = fromInt(x)
    override def fromFloat(x: Float): N = fromDouble(x)
    override def fromDouble(x: Double): N = mp.fromDouble(x)
    override def fromLong(x: Long): N = mp.fromBigInt(x)
    override def fromRational(x: Rational): N = mp.fromRational(x)
    override def fromReal(x: Real): N = mp.fromDouble(x.toDouble)
    override def fromType[B](x: B)(implicit e: ConvertableFrom[B]): N = fromAlgebraic(e.toAlgebraic(x))

    // Members declared in spire.algebra.IsReal
    override def ceil(x: N): N = mp.fromDouble(toDouble(x).ceil)
    override def floor(x: N): N = mp.fromDouble(toDouble(x).floor)
    override def isWhole(x: N): Boolean = new RichMPNumber(x).isIntegral
    override def round(x: N): N = mp.fromBigInt(toDouble(x).round)
    override def toReal(x: N): Real = new RichMPNumber(x).toDouble

    // Members declared in spire.algebra.NRoot
    override def fpow(x: N, y: N): N = new RichMPNumber(x) ** y
    override def nroot(x: N, y: Int): N = mp.proot(x, mp.fromInt(y))

    // Members declared in spire.algebra.Signed
    override def abs(x: N): N = new RichMPNumber(x).abs
    override def signum(x: N): Int = new RichMPNumber(x).signum
  }
}

object MathImports extends MathImports
