package org.newtonpolyhedron
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.BigIntFielded
import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.FracMathVec
package object test {
  def matrInt(content: Array[Array[Int]]): Matrix[BigIntFielded] = {
    Matrix(content map (_ map (x => BigIntFielded(x))))
  }

  def matrFrac(content: Array[Array[Int]]): Matrix[BigFrac] = {
    Matrix(content map (_ map (x => BigFrac(x))))
  }

  def s[T](values: T*): IndexedSeq[T] = IndexedSeq(values: _*)

  def a(values: Int*): Array[Int] = Array(values: _*)
  def a(values: Array[Int]*): Array[Array[Int]] = Array(values: _*)
  def a(values: Array[Array[Int]]*): Array[Array[Array[Int]]] = Array(values: _*)

  def iv(ints: Int*): IntMathVec = IntMathVec(ints: _*)
  def fv(ints: Int*): FracMathVec = FracMathVec.fromInts(ints: _*)
  def fv2(fracs: BigFrac*): FracMathVec = FracMathVec(fracs: _*)

  def bf(n: Int, d: Int) = BigFrac(n, d)
}