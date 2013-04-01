package org.newtonpolyhedron.solve.matrixuni
import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.linear.FieldMatrix
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.Matrix

class UnimodularMatrixMakerImpl extends UnimodularMatrixMaker {

  override def getUnimodularFrom(matrix: FieldMatrix[BigFraction]): FieldMatrix[BigFraction] =
    matrixScala2Java(unimodularFrom(matrix))

  override def unimodularFrom(matrix: Matrix[BigFrac]): Matrix[BigFrac] = {
    require(matrix.isSquare, "Non-square matrix")

    val (matrixDiag, rowOnes, colOnes) = Matrix.toDiagonal(matrix)

    val rowOnesInv = rowOnes.inv
    val colOnesInv = colOnes.inv

    // Sanity check
    assert(matrix == rowOnesInv * matrixDiag * colOnesInv)

    rowOnesInv * colOnesInv
  }
}