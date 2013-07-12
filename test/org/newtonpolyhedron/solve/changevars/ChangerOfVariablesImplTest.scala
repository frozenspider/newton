package org.newtonpolyhedron.solve.changevars

import org.junit.runner.RunWith
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.BigFrac

@RunWith(classOf[JUnitRunner])
class ChangerOfVariablesImplTest extends FunSuite {

  type Polynomial = IndexedSeq[Term]
  type Polys = IndexedSeq[Polynomial]

  val changer = new ChangerOfVariablesImpl

  private def makePoly(components: (Int, Seq[Int])*): Polynomial =
    components map { case (coeff, pows) => Term(Product(coeff), veci(pows)) } toIndexedSeq
  private def veci(s: Seq[Int]) = FracMathVec.fromInts(s: _*)
  private def veci(s: Seq[BigInt]) = FracMathVec.fromInts(s.map(_.toInt): _*)
  private def vecf(s: Seq[BigFrac]) = FracMathVec(s: _*)

  test("artificial example 1") {
    // Verified via Wolfram Alpha
    val poly = makePoly(
      (+2, s(3, 2, 0, 0)),
      (+3, s(0, 0, 2, 2)))
    val substs = s(
      makePoly(
        (+1, s(1, 0)),
        (+1, s(0, 2))),
      makePoly(
        (+1, s(2, 0)),
        (-1, s(0, 1))),
      makePoly(
        (+1, s(3, 0)),
        (+1, s(1, 1))),
      makePoly(
        (+1, s(1, 0)),
        (-2, s(0, 3)))
    )
    val actual = changer.changeVars(poly, substs)

    // Terms:
    // (3 a8), (-12 a7 b3), (2 a7), (12 a6 b6), (6 a6 b2), (6 a6 b), (-18 a5 b4), (-4 a5 b), (24 a4 b7), (2 a4 b6), (-12 a4 b3),
    // (3 a4 b2), (-24 a3 b5), (2 a3 b2), (12 a2 b8), (-4 a2 b7), (6 a2 b4), (6 a b6), (2 b8)
    val expected = makePoly(
      (2, s(3, 2)),
      (-4, s(5, 1)),
      (3, s(4, 2)),
      (6, s(2, 4)),
      (2, s(7, 0)),
      (6, s(6, 1)),
      (-12, s(4, 3)),
      (6, s(1, 6)),
      (3, s(8, 0)),
      (6, s(6, 2)),
      (-24, s(3, 5)),
      (2, s(0, 8)),
      (-18, s(5, 4)),
      (-4, s(2, 7)),
      (-12, s(7, 3)),
      (2, s(4, 6)),
      (12, s(2, 8)),
      (24, s(4, 7)),
      (12, s(6, 6)))
    assert(expected === actual, s"\n Not found:  ${expected diff actual}\nUnexpected: ${actual diff expected}")
  }
}