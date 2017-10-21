package org.newtonpolyhedron.solve.matrixuni

import org.newtonpolyhedron.NewtonImports._

import spire.math.Rational

class UnimodularMatrixMakerImpl[N <: MPNumber](implicit mp: MathProcessor[N]) extends UnimodularMatrixMaker[N] {

  override def unimodularFrom(matrix: Matrix[N]): Matrix[N] = {
    require(matrix.isSquare, "Non-square matrix")

    val (matrixDiag, rowOnes, colOnes) = mp.diagonalize(matrix)

    val rowOnesInv = rowOnes.inv
    val colOnesInv = colOnes.inv

    // Sanity check
    assert(matrix == rowOnesInv * matrixDiag * colOnesInv)

    rowOnesInv * colOnesInv
  } ensuring (_.det == mp.one)
}
