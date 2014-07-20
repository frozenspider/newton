package org.newtonpolyhedron.ui

import scala.swing.Component

import org.scilab.forge.jlatexmath.TeXConstants
import org.scilab.forge.jlatexmath.TeXFormula

import javax.swing.JLabel

class LatexRenderingComponent extends Component {

  override lazy val peer = new JLabel

  private var latexString: String = ""

  var fontSize = 16.0f

  def content: String = latexString

  def content_=(latex: String): Unit = {
    this.latexString = latex
    val formula = new TeXFormula(latexString)
    val icon = (new formula.TeXIconBuilder)
      .setStyle(TeXConstants.STYLE_DISPLAY)
      .setSize(fontSize)
      // .setWidth(TeXConstants.UNIT_PIXEL, 256f, TeXConstants.ALIGN_CENTER)
      // .setIsMaxWidth(true)
      // .setInterLineSpacing(TeXConstants.UNIT_PIXEL, 20f)
      .build
    peer.setIcon(icon)
  }
}
