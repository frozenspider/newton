package org.newtonpolyhedron.ui

import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.Dialog
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.Panel
import scala.swing.TextField
import scala.swing.event.ButtonClicked

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.equation.Equation
import org.newtonpolyhedron.entity.equation.EquationSign
import org.newtonpolyhedron.solve.eqsys.EqSystemSolutionInput
import org.newtonpolyhedron.ui.eqsys.EqSystemRenderingPanel

class EqSystemSolverDialogInput extends EqSystemSolutionInput {
  import BorderPanel.Position._
  import org.newtonpolyhedron.conversion.latex._

  private val varName = "x"

  //
  // UI
  //
  private lazy val inputDialog = new Dialog {
    def resetDialog(headerText: String, inputPanels: Seq[Panel], eqSys: Equations) = {
      ok = false
      header.text = headerText
      inputsContainer.contents.clear()
      inputsContainer.contents ++= inputPanels
      eqSysRenderer.render(eqSys, varName)
      pack()
      centerOnScreen()
    }
    var ok = false
    modal = true
    title = "Manual solver"
    private val header = new Label
    private val inputsContainer = new FlowPanel
    private val eqSysRenderer = new EqSystemRenderingPanel
    private val okBtn = new Button("OK")
    private val cnBtn = new Button("Cancel")
    contents = new BorderPanel {
      layout(header) = North
      layout(eqSysRenderer) = Center
      layout(new BorderPanel {
        layout(inputsContainer) = Center
        layout(new FlowPanel {
          contents ++= Seq(okBtn, cnBtn)
        }) = South
      }) = South
      listenTo(okBtn, cnBtn)
      reactions += {
        case ButtonClicked(`okBtn`) =>
          ok = true
          dispose()
        case ButtonClicked(`cnBtn`) =>
          dispose()
      }
    }
  }

  private def createInputField = new TextField { columns = 5 }

  private def createInputComponents(upTo: Int): (Seq[TextField], Seq[Panel]) =
    (1 to upTo).map(i => {
      val inputField = createInputField
      val panel = new BorderPanel {
        layout(
          new LatexRenderingComponent {
            content = latex(variableToLatex(varName, Some(i)) + "=")
          }
        ) = West
        layout(inputField) = Center
      }
      (inputField, panel)
    }).unzip

  private def asEqualsZeroEquation(dim: Int) = Equation(_: Polynomial, EquationSign.Equals, zeroPoly(dim))

  override def getInputFor(system: Polys, headerTextOption: Option[String]): Option[Seq[String]] = {
    val dim = system collectFirst {
      case poly if !poly.isEmpty => poly.head.powers.dim
    } getOrElse (throw new IllegalArgumentException("Can't get dimension of polys"))

    val (inputs, inputPanels) = createInputComponents(dim)

    inputDialog.resetDialog(
      headerText = headerTextOption getOrElse s"Please solve for $varName:",
      inputPanels = inputPanels,
      eqSys = system map asEqualsZeroEquation(dim)
    )
    inputDialog.visible = true
    inputDialog.dispose

    if (inputDialog.ok) {
      Some(inputs map (_.text.trim))
    } else {
      None
    }
  }

}

object EqSystemSolverDialogInput extends App {
  import org.newtonpolyhedron._
  import org.newtonpolyhedron.entity._
  import org.newtonpolyhedron.entity.vector._
  import org.newtonpolyhedron.entity.equation._

  val s = new EqSystemSolverDialogInput

  val tricky = EqSystemRenderingPanel.tricky

  val eqs: Polys = IndexedSeq[Polynomial](
    IndexedSeq(
      new Term(Product(1), FracMathVec(1, 2, 3)),
      new Term(tricky, FracMathVec(1, 2, 3)),
      new Term(Product(4), FracMathVec(0, 0, 0))
    ),
    IndexedSeq(
      new Term(Product(BigFrac(-1, 2)), FracMathVec(BigFrac(-1, 2), BigFrac.ZERO, BigFrac(-333, 667))),
      new Term(Product(-2), FracMathVec(0, 0, 3))
    )
  )

  println(s.getInputFor(eqs, None))
}
