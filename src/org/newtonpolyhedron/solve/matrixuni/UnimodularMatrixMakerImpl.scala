package org.newtonpolyhedron.solve.matrixuni
import org.newtonpolyhedron._
import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.fraction.BigFraction
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.utils.MatrixUtils
import org.newtonpolyhedron.entity.BigFrac.BigFracField
import org.apache.commons.math3.Field
import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.fraction.BigFractionField
import org.newtonpolyhedron.entity.Matrix

class UnimodularMatrixMakerImpl extends UnimodularMatrixMaker {

  override def getUnimodularFrom(matrix: FieldMatrix[BigFraction]): FieldMatrix[BigFraction] =
    unimodularFrom(matrix)

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