package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.NewtonImports._

import spire.math.Rational

class UnimodularMatrixMakerImpl[N <: MPNumber, M <: MPMatrix](implicit mp: MathProcessor[N, M]) extends UnimodularMatrixMaker[N, M] {

  override def unimodularFrom(matrix: M): M = {
    require(matrix.isSquare, "Non-square matrix")

    val (matrixDiag, rowOnes, colOnes) = matrix.diagonalize

    val rowOnesInv = rowOnes.inverse
    val colOnesInv = colOnes.inverse

    // Sanity check
    assert(matrix == rowOnesInv * matrixDiag * colOnesInv)

    rowOnesInv * colOnesInv
  } //ensuring (_.det == mp.one)
}
