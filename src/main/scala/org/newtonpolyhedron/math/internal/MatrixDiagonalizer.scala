package org.newtonpolyhedron.math.internal

import scala.annotation.tailrec
import org.apache.commons.math3.linear.MatrixUtils
import org.newtonpolyhedron.math.internal.FieldElementSupport._
import org.newtonpolyhedron.utils.LanguageImplicits._
import spire.math.Rational
import org.newtonpolyhedron.math.MathImports._

/**
 * @see MatrixMathProcessor.diagonalize
 * @author FS
 */
object MatrixDiagonalizer {
  private sealed trait Orientation
  private case object ROWS extends Orientation
  private case object COLS extends Orientation

  private type AM = ApacheMatrix[Rational]
  private val AM = ApacheMatrix
  private type MatrixTriple[N] = (Matrix[N], Matrix[N], Matrix[N])
  private type AMTriple[N] = (AM, AM, AM)

  def diagonalize(mt: Matrix[Rational]): MatrixTriple[Rational] = {
    require(mt.isSquare, "Non-square matrix")
    val iden = AM.from(Matrix.idenitiy[Rational](mt.rowCount))
    val rowOnes = new AM(iden.contentCopy)(RationalFieldWrapper)
    val colOnes = new AM(iden.contentCopy)(RationalFieldWrapper)
    val amTriple = FunctionalMatrixCompanion.toDiagonalTriple((AM.from(mt), rowOnes, colOnes))
    (AM.to(amTriple._1), AM.to(amTriple._2), AM.to(amTriple._3))
  }

  private implicit class FunctionalApacheMatrix(val mt: AM) {

    def getCornerRelative(orientation: Orientation, cornerIdx: Int, colIdx: Int): Rational =
      orientation match {
        case ROWS => mt(cornerIdx, colIdx)
        case COLS => mt(colIdx, cornerIdx)
      }

    def swapInverse(i: Int, j: Int, orientation: Orientation): AM = orientation match {
      case ROWS => mt.swapCols(i, j).inverseCol(j)
      case COLS => mt.swapRows(i, j).inverseRow(j)
    }

    def swapRows(i: Int, j: Int): AM = {
      mt.transpose.swapCols(i, j).transpose
    }

    def swapCols(i: Int, j: Int): AM = {
      val copy = mt.contentCopy
      val col1 = copy.getColumnVector(i)
      val col2 = copy.getColumnVector(j)
      copy.setColumnVector(i, col2)
      copy.setColumnVector(j, col1)
      new AM(copy)(RationalFieldWrapper)
    }

    def fieldRowOfZeros(size: Int) =
      MatrixUtils.createFieldMatrix(mt.field, size, 1).getColumnVector(0)

    def inverseRow(i: Int): AM = {
      mt.transpose.inverseCol(i).transpose
    }

    def inverseCol(i: Int): AM = {
      val copy = mt.contentCopy
      val vec = copy.getColumnVector(i)
      val zeros = fieldRowOfZeros(vec.getDimension)
      copy.setColumnVector(i, zeros subtract vec)
      new AM(copy)(RationalFieldWrapper)
    }

    def subtractMultiplied(from: Int, to: Int, quotient: Rational, orientation: Orientation): AM =
      orientation match {
        case ROWS => mt.subtractMultipliedCol(from, to, quotient)
        case COLS => mt.subtractMultipliedRow(from, to, quotient)
      }

    def subtractMultipliedRow(from: Int, to: Int, quotient: Rational): AM = {
      mt.transpose.subtractMultipliedCol(from, to, quotient).transpose
    }

    def subtractMultipliedCol(from: Int, to: Int, quotient: Rational): AM = {
      val copy = mt.contentCopy
      val srcCol = copy.getColumnVector(from)
      val dstCol = copy.getColumnVector(to)
      val srcColMul = srcCol mapMultiply RationalFieldWrapper(quotient)
      val dstColSub = dstCol subtract srcColMul
      copy.setColumnVector(to, dstColSub)
      new AM(copy)(RationalFieldWrapper)
    }
  }

  //
  //
  //

  private implicit class FunctionalMatrixTriple(val mt: AMTriple[Rational]) {
    val (main, rowOnes, colOnes) = mt

    def transpose: AMTriple[Rational] = {
      (
        main.transpose,
        colOnes.transpose,
        rowOnes.transpose
      )
    }

    def map[R](f: AM => R): (R, R, R) = {
      (f(main), f(rowOnes), f(colOnes))
    }

    def swapInverse(i: Int, j: Int, orientation: Orientation): AMTriple[Rational] = {
      val distance = math.abs(i - j)
      orientation match {
        case ROWS => mt.swapCols(i, j).inverseCol(j)
        case COLS => mt.swapRows(i, j).inverseRow(j)
      }
    }

    def swapRows(i: Int, j: Int): AMTriple[Rational] = {
      (
        main.swapRows(i, j),
        rowOnes.swapRows(i, j),
        colOnes
      )
    }

    def swapCols(i: Int, j: Int): AMTriple[Rational] = {
      (
        main.swapCols(i, j),
        rowOnes,
        colOnes.swapCols(i, j)
      )
    }

    def inverseRow(i: Int): AMTriple[Rational] = {
      (
        main.inverseRow(i),
        rowOnes.inverseRow(i),
        colOnes
      )
    }

    def inverseCol(i: Int): AMTriple[Rational] = {
      (
        main.inverseCol(i),
        rowOnes,
        colOnes.inverseCol(i)
      )
    }

    def subtractMultiplied(from: Int, to: Int, quotient: Rational, orientation: Orientation): AMTriple[Rational] = {
      orientation match {
        case ROWS => mt.subtractMultipliedCol(from, to, quotient)
        case COLS => mt.subtractMultipliedRow(from, to, quotient)
      }
    }

    def subtractMultipliedRow(from: Int, to: Int, quotient: Rational): AMTriple[Rational] = {
      (
        main.subtractMultipliedRow(from, to, quotient),
        rowOnes.subtractMultipliedRow(from, to, quotient),
        colOnes
      )
    }

    def subtractMultipliedCol(from: Int, to: Int, quotient: Rational): AMTriple[Rational] = {
      (
        main.subtractMultipliedCol(from, to, quotient),
        rowOnes,
        colOnes.subtractMultipliedCol(from, to, quotient)
      )
    }
  }

  //
  //
  //

  private object FunctionalMatrixCompanion {
    def toDiagonalTriple(mts: AMTriple[Rational]): AMTriple[Rational] = {
      toDiagonalTriple(mts, 0)
    }

    def toDiagonalTriple(mts: AMTriple[Rational], cornerIdx: Int): AMTriple[Rational] = {
      val dim = mts.main.rowCount
      if (cornerIdx == dim - 1) mts
      else {
        // Put GCD into corner
        val res1 = processUntilCornerIsGCD(mts, cornerIdx)
        val corner = res1.main(cornerIdx, cornerIdx)
        // Do linear transformations until row and col are all-but-corner zeros
        val res2 = ((cornerIdx + 1) until dim).foldLeft(res1) { (mts, colIdx) =>
          val current = mts.main.getCornerRelative(ROWS, cornerIdx, colIdx)
          val quotient = (current / corner).quotient
          if (quotient != 0) {
            mts.subtractMultiplied(cornerIdx, colIdx, Rational(quotient), ROWS)
          } else {
            mts
          }
        }
        val res3 = ((cornerIdx + 1) until dim).foldLeft(res2) { (mts, rowIdx) =>
          val current = mts.main.getCornerRelative(COLS, cornerIdx, rowIdx)
          val quotient = (current / corner).quotient
          if (quotient != 0) {
            mts.subtractMultiplied(cornerIdx, rowIdx, Rational(quotient), COLS)
          } else {
            mts
          }
        }
        toDiagonalTriple(res3, cornerIdx + 1)
      }
    }

    def processUntilCornerIsGCD(mts: AMTriple[Rational], cornerIdx: Int): AMTriple[Rational] = {
      val (resRows, rowsHadRem) = tryPutGCDIntoCorner(mts, ROWS, cornerIdx)
      val (resCols, colsHadRem) = tryPutGCDIntoCorner(resRows, COLS, cornerIdx)
      if (rowsHadRem || colsHadRem)
        processUntilCornerIsGCD(resCols, cornerIdx)
      else
        resCols
    }

    def tryPutGCDIntoCorner(
        mts:         AMTriple[Rational],
        orientation: Orientation,
        cornerIdx:   Int
    ): (AMTriple[Rational], Boolean) = {
      val dim = mts.main.rowCount
      if (cornerIdx == dim - 1) {
        (mts, false)
      } else {
        tryPutGCDIntoCornerFrom(orientation, cornerIdx)(mts, cornerIdx + 1, false)
      }
    }

    def tryPutGCDIntoCornerFrom(
        orientation: Orientation,
        cornerIdx:   Int
    )(mts: AMTriple[Rational], currIdx: Int, hadRemainder: Boolean): (AMTriple[Rational], Boolean) = {
      @tailrec
      def recurse(mts: AMTriple[Rational], currIdx: Int, hadRemainder: Boolean): (AMTriple[Rational], Boolean) = {
        val dim = mts.main.rowCount
        val corner = mts.main(cornerIdx, cornerIdx)
        if (currIdx == dim) {
          (mts, hadRemainder)
        } else {
          val current = mts.main.getCornerRelative(orientation, cornerIdx, currIdx)
          if (current == 0) {
            recurse(mts, currIdx + 1, hadRemainder)
          } else {
            if (corner == 0) {
              val mts2 = mts swapInverse (cornerIdx, currIdx, orientation)
              recurse(mts2, cornerIdx + 1, hadRemainder)
            } else {
              val div = current / corner
              if (div.remainder != 0) {
                val mts2 = if (div.quotient != 0) {
                  mts subtractMultiplied (cornerIdx, currIdx, Rational(div.quotient), orientation)
                } else {
                  mts
                }
                val mts3 = mts2 swapInverse (cornerIdx, currIdx, orientation)
                recurse(mts3, currIdx, true)
              } else {
                recurse(mts, currIdx + 1, hadRemainder)
              }
            }
          } // -- current != 0
        }
      }
      recurse(mts, currIdx, hadRemainder)
    }
  }
}
