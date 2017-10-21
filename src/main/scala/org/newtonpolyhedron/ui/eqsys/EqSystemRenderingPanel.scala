package org.newtonpolyhedron.ui.eqsys

import scala.swing.BorderPanel

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.ui.LatexRenderingComponent
import org.newtonpolyhedron.utils.LatexConversion
import org.newtonpolyhedron.utils.PolynomialUtils.Equations

class EqSystemRenderingPanel extends BorderPanel {
  import scala.swing.BorderPanel.Position._

  private val renderer = new LatexRenderingComponent
  renderer.fontSize = 30

  layout(renderer) = Center

  def fontSize = renderer.fontSize

  def fontSize_=(f: Float): Unit = renderer.fontSize = f

  def render[N <: MPNumber](eqs: Equations[N], varName1: String, varName2: String = "Var")(implicit mp: MathProcessor[N]) = {
    renderer.content = LatexConversion.equationsToLatex(eqs, varName1, varName2)
  }
}

object EqSystemRenderingPanel extends scala.swing.SimpleSwingApplication {
  import org.newtonpolyhedron.entity.equation._
  import org.newtonpolyhedron.math.internal.InternalMathProcessor
  import org.newtonpolyhedron.math.internal.Product
  import scala.swing.MainFrame
  import spire.implicits._
  import spire.math.Rational

  val subj = new EqSystemRenderingPanel
  override def top = new MainFrame {
    contents = subj
  }
  private type N = Product
  private implicit val mp: MathProcessor[N] = new InternalMathProcessor

  val tricky = Product(1, Map(2 -> Rational(15), 3 -> Rational(3), 5 -> Rational(8))) ** mp.fromRational(Rational(1, 6))

  val eqs: Equations[N] = IndexedSeq[Equation[N]](
    Equation(
      IndexedSeq(
        new Term(Product(1), NumVec[N](1, 2, 3)),
        new Term(Product(0), NumVec[N](1, 2, 3)),
        new Term(Product(4), NumVec[N](0, 0, 0))
      ),
      EquationSign.Equals,
      IndexedSeq(
        new Term(Product(1), NumVec[N](1, 2, 3)),
        new Term(tricky, NumVec[N](1, 2, 3)),
        new Term(Product(4), NumVec[N](0, 0, 0))
      )
    ),
    Equation(
      IndexedSeq(
        new Term(
          mp.fromRational(Rational(-1, 2)),
          NumVec[N](mp.fromRational(Rational(-1, 2)), mp.zero, mp.fromRational(Rational(-333, 667)))
        ),
        new Term(
          mp.fromInt(-2),
          NumVec[N](0, 0, 3)
        )
      ),
      EquationSign.GreaterEq,
      zeroPoly(3)
    )
  )

  subj.render(eqs, "x", "y")
}
