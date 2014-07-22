package org.newtonpolyhedron.ui.eqsys

import scala.swing._
import org.newtonpolyhedron.Equations
import org.newtonpolyhedron.conversion.latex.equationsToLatex
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.BigFrac.int2bigFrac
import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.equation.Equation
import org.newtonpolyhedron.entity.equation.EquationSign
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.ui.LatexRenderingComponent
import org.newtonpolyhedron.zeroPoly
import scala.IndexedSeq
import scala.swing.BorderPanel.Position.Center

class EqSystemRenderingPanel extends BorderPanel {
  import BorderPanel.Position._
  import org.newtonpolyhedron.conversion.latex._

  private val renderer = new LatexRenderingComponent
  renderer.fontSize = 30

  layout(renderer) = Center

  def fontSize = renderer.fontSize

  def fontSize_=(f: Float): Unit = renderer.fontSize = f

  def render(eqs: Equations, varName1: String, varName2: String = "Var") = {
    renderer.content = equationsToLatex(eqs, varName1, varName2)
  }
}

object EqSystemRenderingPanel extends SimpleSwingApplication {
  import org.newtonpolyhedron.entity.equation.Equation
  import org.newtonpolyhedron._
  import org.newtonpolyhedron.entity._
  import org.newtonpolyhedron.entity.vector._
  import org.newtonpolyhedron.entity.equation.EquationSign

  val subj = new EqSystemRenderingPanel
  def top = new MainFrame {
    contents = subj
  }

  val tricky = Product(1, Map(2 -> BigFrac(15), 3 -> BigFrac(3), 5 -> BigFrac(8))).pow(BigFrac(1, 6))
  println(tricky.toStructuredString)

  val eqs: Equations = IndexedSeq[Equation](
    Equation(
      IndexedSeq(
        new Term(Product(1), FracMathVec(1, 2, 3)),
        new Term(Product(0), FracMathVec(1, 2, 3)),
        new Term(Product(4), FracMathVec(0, 0, 0))
      ),
      EquationSign.Equals,
      IndexedSeq(
        new Term(Product(1), FracMathVec(1, 2, 3)),
        new Term(tricky, FracMathVec(1, 2, 3)),
        new Term(Product(4), FracMathVec(0, 0, 0))
      )
    ),
    Equation(
      IndexedSeq(
        new Term(Product(BigFrac(-1, 2)), FracMathVec(BigFrac(-1, 2), BigFrac.ZERO, BigFrac(-333, 667))),
        new Term(Product(-2), FracMathVec(0, 0, 3))
      ),
      EquationSign.GreaterEq,
      zeroPoly(3)
    )
  )

  subj.render(eqs, "x", "y")
}
