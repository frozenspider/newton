package org.newtonpolyhedron.playground

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.math.internal.InternalMathProcessor
import org.newtonpolyhedron.math.internal.Product
import org.newtonpolyhedron.ui.eqsys.EqSystemSolutionDialogInput

import spire.implicits._
import spire.math.Rational

object EqSystemDialogPlayground extends App {

  private type N = Product
  private implicit val mp: MathProcessor[N] = new InternalMathProcessor

  val s = new EqSystemSolutionDialogInput[N]

  val tricky = EqSystemRenderingPlayground.tricky

  val eqs: Polys[N] = IndexedSeq[Polynomial[N]](
    IndexedSeq(
      new Term(Product(1), NumVec[N](1, 2, 3)),
      new Term(tricky, NumVec[N](1, 2, 3)),
      new Term(Product(4), NumVec[N](0, 0, 0))
    ),
    IndexedSeq(
      new Term(
        mp.fromRational(Rational(-1, 2)),
        NumVec[N](mp.fromRational(Rational(-1, 2)), mp.zero, mp.fromRational(Rational(-333, 667)))
      ),
      new Term(
        mp.fromInt(-2),
        NumVec[N](0, 0, 3)
      )
    )
  )

  println(s.getInputFor(eqs, None, None))
}
