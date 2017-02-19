package org.newtonpolyhedron.entity.matrix

import scala.annotation.tailrec

import org.apache.commons.math3.linear.MatrixUtils
import org.newtonpolyhedron.entity.BigFrac

import internal.FieldElementSupport._

/**
 * Provides implicit "toDiagonal" method on BigFrac matrices, which is needed for unimodular matrices maker
 * via Euler (or whatever is its name, not sure yet) algorithm.
 */
object MatrixToDiagonalImplicits {
  private sealed trait Orientation
  private case object ROWS extends Orientation
  private case object COLS extends Orientation

  private type M = Matrix[BigFrac]
  private type MatrixTriple = (M, M, M)

  implicit class FunctionalMatrix(val mt: M) {

    /**
     * Converts the matrix to diagonal form.
     * <p>
     * Returns diagonal matrix alongside with row and column transformation matrices
     */
    def toDiagonal: MatrixTriple = {
      require(mt.isSquare, "Non-square matrix")
      val iden = Matrix.idenitiy[BigFrac](mt.rowCount)
      val rowOnes = new Matrix(iden.contentCopy)(bigFracFieldWrapper)
      val colOnes = new Matrix(iden.contentCopy)(bigFracFieldWrapper)
      FunctionalMatrixCompanion.toDiagonalTriple((mt, rowOnes, colOnes))
    }

    private[matrix] def getCornerRelative(orientation: Orientation, cornerIdx: Int, colIdx: Int): BigFrac =
      orientation match {
        case ROWS => mt(cornerIdx, colIdx)
        case COLS => mt(colIdx, cornerIdx)
      }

    private def swapInverse(i: Int, j: Int, orientation: Orientation): M = orientation match {
      case ROWS => mt.swapCols(i, j).inverseCol(j)
      case COLS => mt.swapRows(i, j).inverseRow(j)
    }

    private[matrix] def swapRows(i: Int, j: Int): M = {
      mt.transpose.swapCols(i, j).transpose
    }

    private[matrix] def swapCols(i: Int, j: Int): M = {
      val copy = mt.contentCopy
      val col1 = copy.getColumnVector(i)
      val col2 = copy.getColumnVector(j)
      copy.setColumnVector(i, col2)
      copy.setColumnVector(j, col1)
      new Matrix(copy)(bigFracFieldWrapper)
    }

    private def fieldRowOfZeros(size: Int) =
      MatrixUtils.createFieldMatrix(mt.field, size, 1).getColumnVector(0)

    private[matrix] def inverseRow(i: Int): M = {
      mt.transpose.inverseCol(i).transpose
    }

    private[matrix] def inverseCol(i: Int): M = {
      val copy = mt.contentCopy
      val vec = copy.getColumnVector(i)
      val zeros = fieldRowOfZeros(vec.getDimension)
      copy.setColumnVector(i, zeros subtract vec)
      new Matrix(copy)(bigFracFieldWrapper)
    }

    private[matrix] def subtractMultiplied(from: Int, to: Int, quotient: BigFrac, orientation: Orientation): M =
      orientation match {
        case ROWS => mt.subtractMultipliedCol(from, to, quotient)
        case COLS => mt.subtractMultipliedRow(from, to, quotient)
      }

    private[matrix] def subtractMultipliedRow(from: Int, to: Int, quotient: BigFrac): M = {
      mt.transpose.subtractMultipliedCol(from, to, quotient).transpose
    }

    private[matrix] def subtractMultipliedCol(from: Int, to: Int, quotient: BigFrac): M = {
      val copy = mt.contentCopy
      val srcCol = copy.getColumnVector(from)
      val dstCol = copy.getColumnVector(to)
      val srcColMul = srcCol mapMultiply bigFracFieldWrapper(quotient)
      val dstColSub = dstCol subtract srcColMul
      copy.setColumnVector(to, dstColSub)
      new Matrix(copy)(bigFracFieldWrapper)
    }
  }

  //
  //
  //

  private implicit class FunctionalMatrixTriple(val mt: MatrixTriple) {
    private[matrix] val (main, rowOnes, colOnes) = mt

    private def transpose: MatrixTriple = {
      (
        main.transpose,
        colOnes.transpose,
        rowOnes.transpose
      )
    }

    private def map[R](f: M => R): (R, R, R) = {
      (f(main), f(rowOnes), f(colOnes))
    }

    private[matrix] def swapInverse(i: Int, j: Int, orientation: Orientation): MatrixTriple = {
      val distance = math.abs(i - j)
      orientation match {
        case ROWS => mt.swapCols(i, j).inverseCol(j)
        case COLS => mt.swapRows(i, j).inverseRow(j)
      }
    }

    private[FunctionalMatrixTriple] def swapRows(i: Int, j: Int): MatrixTriple = {
      (
        main.swapRows(i, j),
        rowOnes.swapRows(i, j),
        colOnes
      )
    }

    private[FunctionalMatrixTriple] def swapCols(i: Int, j: Int): MatrixTriple = {
      (
        main.swapCols(i, j),
        rowOnes,
        colOnes.swapCols(i, j)
      )
    }

    private[FunctionalMatrixTriple] def inverseRow(i: Int): MatrixTriple = {
      (
        main.inverseRow(i),
        rowOnes.inverseRow(i),
        colOnes
      )
    }

    private[FunctionalMatrixTriple] def inverseCol(i: Int): MatrixTriple = {
      (
        main.inverseCol(i),
        rowOnes,
        colOnes.inverseCol(i)
      )
    }

    private[matrix] def subtractMultiplied(from: Int, to: Int, quotient: BigFrac, orientation: Orientation): MatrixTriple = {
      orientation match {
        case ROWS => mt.subtractMultipliedCol(from, to, quotient)
        case COLS => mt.subtractMultipliedRow(from, to, quotient)
      }
    }

    private[FunctionalMatrixTriple] def subtractMultipliedRow(from: Int, to: Int, quotient: BigFrac): MatrixTriple = {
      (
        main.subtractMultipliedRow(from, to, quotient),
        rowOnes.subtractMultipliedRow(from, to, quotient),
        colOnes
      )
    }

    private[FunctionalMatrixTriple] def subtractMultipliedCol(from: Int, to: Int, quotient: BigFrac): MatrixTriple = {
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
    def toDiagonalTriple(mts: MatrixTriple): MatrixTriple = {
      toDiagonalTriple(mts, 0)
    }

    private def toDiagonalTriple(mts: MatrixTriple, cornerIdx: Int): MatrixTriple = {
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
            mts.subtractMultiplied(cornerIdx, colIdx, BigFrac(quotient), ROWS)
          } else {
            mts
          }
        }
        val res3 = ((cornerIdx + 1) until dim).foldLeft(res2) { (mts, rowIdx) =>
          val current = mts.main.getCornerRelative(COLS, cornerIdx, rowIdx)
          val quotient = (current / corner).quotient
          if (quotient != 0) {
            mts.subtractMultiplied(cornerIdx, rowIdx, BigFrac(quotient), COLS)
          } else {
            mts
          }
        }
        toDiagonalTriple(res3, cornerIdx + 1)
      }
    }

    private def processUntilCornerIsGCD(mts: MatrixTriple, cornerIdx: Int): MatrixTriple = {
      val (resRows, rowsHadRem) = tryPutGCDIntoCorner(mts, ROWS, cornerIdx)
      val (resCols, colsHadRem) = tryPutGCDIntoCorner(resRows, COLS, cornerIdx)
      if (rowsHadRem || colsHadRem)
        processUntilCornerIsGCD(resCols, cornerIdx)
      else
        resCols
    }

    private def tryPutGCDIntoCorner(mts: MatrixTriple, orientation: Orientation, cornerIdx: Int): (MatrixTriple, Boolean) = {
      val dim = mts.main.rowCount
      if (cornerIdx == dim - 1) {
        (mts, false)
      } else {
        tryPutGCDIntoCornerFrom(orientation, cornerIdx)(mts, cornerIdx + 1, false)
      }
    }

    private def tryPutGCDIntoCornerFrom(orientation: Orientation,
                                        cornerIdx: Int)(mts: MatrixTriple, currIdx: Int, hadRemainder: Boolean): (MatrixTriple, Boolean) = {
      @tailrec
      def recurse(mts: MatrixTriple, currIdx: Int, hadRemainder: Boolean): (MatrixTriple, Boolean) = {
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
                  mts subtractMultiplied (cornerIdx, currIdx, BigFrac(div.quotient), orientation)
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
