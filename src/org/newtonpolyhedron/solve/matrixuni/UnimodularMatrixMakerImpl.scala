package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix

class UnimodularMatrixMakerImpl extends UnimodularMatrixMaker {

  override def unimodularFrom(matrix: Matrix[BigFrac]): Matrix[BigFrac] = {
    require(matrix.isSquare, "Non-square matrix")

    val (matrixDiag, rowOnes, colOnes) = Matrix.toDiagonal(matrix)

    val rowOnesInv = rowOnes.inv
    val colOnesInv = colOnes.inv

    // Sanity check
    assert(matrix == rowOnesInv * matrixDiag * colOnesInv)

    rowOnesInv * colOnesInv
  } ensuring (_.det == 1)
}