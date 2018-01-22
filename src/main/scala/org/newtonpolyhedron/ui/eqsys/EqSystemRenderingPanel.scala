package org.newtonpolyhedron.ui.eqsys

import scala.swing.BorderPanel

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.ui.LatexRenderingComponent
import org.newtonpolyhedron.utils.LatexConversion

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
