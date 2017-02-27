package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.entity.matrix.Matrix
import org.newtonpolyhedron.entity.matrix.MatrixToDiagonalImplicits._
import spire.math.Rational

class UnimodularMatrixMakerImpl extends UnimodularMatrixMaker {

  override def unimodularFrom(matrix: Matrix[Rational]): Matrix[Rational] = {
    require(matrix.isSquare, "Non-square matrix")

    val (matrixDiag, rowOnes, colOnes) = matrix.toDiagonal

    val rowOnesInv = rowOnes.inv
    val colOnesInv = colOnes.inv

    // Sanity check
    assert(matrix == rowOnesInv * matrixDiag * colOnesInv)

    rowOnesInv * colOnesInv
  } ensuring (_.det == 1)
}
