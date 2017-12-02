package org.newtonpolyhedron.ui

import scala.swing.Component

import org.newtonpolyhedron.utils.LatexConversion._
import org.scilab.forge.jlatexmath.TeXConstants
import org.scilab.forge.jlatexmath.TeXFormula

import javax.swing.JLabel

/**
 * Component for rendering LaTeX-formatter content.
 *
 * Relies on `JLaTeXMath` for the actual rendering.
 *
 * @author FS
 */
class LatexRenderingComponent extends Component {

  override lazy val peer = new JLabel

  var fontSize = 16.0f

  private var latexString: LatexString = "".asInstanceOf[LatexString]

  def content: LatexString = latexString

  def content_=(latex: LatexString): Unit = {
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
