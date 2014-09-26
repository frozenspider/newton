package org.newtonpolyhedron.ui.eqsys

import scala.swing.BorderPanel

import org.newtonpolyhedron.Equations
import org.newtonpolyhedron.ui.LatexRenderingComponent

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

object EqSystemRenderingPanel extends scala.swing.SimpleSwingApplication {
  import scala.swing.MainFrame
  import org.newtonpolyhedron._
  import org.newtonpolyhedron.entity._
  import org.newtonpolyhedron.entity.vector.VectorImports._
  import org.newtonpolyhedron.entity.equation._

  val subj = new EqSystemRenderingPanel
  def top = new MainFrame {
    contents = subj
  }

  val tricky = Product(1, Map(2 -> BigFrac(15), 3 -> BigFrac(3), 5 -> BigFrac(8))).pow(BigFrac(1, 6))

  val eqs: Equations = IndexedSeq[Equation](
    Equation(
      IndexedSeq(
        new Term(Product(1), FracVec(1, 2, 3)),
        new Term(Product(0), FracVec(1, 2, 3)),
        new Term(Product(4), FracVec(0, 0, 0))
      ),
      EquationSign.Equals,
      IndexedSeq(
        new Term(Product(1), FracVec(1, 2, 3)),
        new Term(tricky, FracVec(1, 2, 3)),
        new Term(Product(4), FracVec(0, 0, 0))
      )
    ),
    Equation(
      IndexedSeq(
        new Term(Product(BigFrac(-1, 2)), FracVec(BigFrac(-1, 2), BigFrac.ZERO, BigFrac(-333, 667))),
        new Term(Product(-2), FracVec(0, 0, 3))
      ),
      EquationSign.GreaterEq,
      zeroPoly(3)
    )
  )

  subj.render(eqs, "x", "y")
}
