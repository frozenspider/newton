package org.newtonpolyhedron
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.BigIntFielded

import org.newtonpolyhedron.entity.Matrix
package object test {
  def matrInt(content: Array[Array[Int]]): Matrix[BigIntFielded] = {
    Matrix(content map (_ map (x => BigIntFielded(x))))
  }

  def matrFrac(content: Array[Array[Int]]): Matrix[BigFrac] = {
    Matrix(content map (_ map (x => BigFrac(x))))
  }

  def a(values: Int*): Array[Int] = Array[Int](values: _*)
  def a(values: Array[Int]*): Array[Array[Int]] = Array[Array[Int]](values: _*)
}