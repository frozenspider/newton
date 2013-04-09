package org.newtonpolyhedron.entity.vector

trait VectorFormat[C <: Ordered[C], V <: MathVector[C, V]] {
  def createArrayOfZeros(length: Int): Array[C]

  def parseElement(src: String): C

  def makeVector(components: Seq[C]): V
}