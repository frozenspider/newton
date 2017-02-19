package org.newtonpolyhedron

object WorkingMode extends Enumeration {
  type WorkingMode = Value

  val Poly = Value("Polyhedron")
  val PolyIntersection = Value("Polyhedron Intersection")
  val Cone = Value("Cone (Motzkin - Burger)")
  val MatrixDet = Value("Matrix Determinant")
  val MatrixInv = Value("Matrix Inverse")
  val MatrixUnimodularAlpha = Value("Unimodular \"Alpha\"-matrix")
  val MatrixLastRowMinorsGCD = Value("Matrix last row minors GCD")
  val PowerTransformation = Value("Power Transformation")
}

