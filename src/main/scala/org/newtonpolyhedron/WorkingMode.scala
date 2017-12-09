package org.newtonpolyhedron

object WorkingMode extends Enumeration {
  type WorkingMode = Value

  val Cone = Value("Cone (Motzkin - Burger)")
  val Poly = Value("Polyhedron")
  val PolyIntersection = Value("Polyhedron Cones Intersection")
  val MatrixDet = Value("Matrix Determinant")
  val MatrixInv = Value("Matrix Inverse")
  val MatrixUnimodularAlpha = Value("Unimodular \"Alpha\"-matrix")
  val MatrixLastRowMinorsGCD = Value("Matrix last row minors GCD")
  val PowerTransformation = Value("Power Transformation")
}

