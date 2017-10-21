package org.newtonpolyhedron.ui.eqsys

import scala.swing.BorderPanel
import scala.swing.Button
import scala.swing.Dialog
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.Panel
import scala.swing.TextField
import scala.swing.event.ButtonClicked

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.equation.Equation
import org.newtonpolyhedron.entity.equation.EquationSign
import org.newtonpolyhedron.solve.eqsys.EqSystemSolutionInput
import org.newtonpolyhedron.ui.LatexRenderingComponent

class EqSystemSolutionDialogInput[N <: MPNumber](implicit mp: MathProcessor[N]) extends EqSystemSolutionInput[N] {
  import BorderPanel.Position._
  import org.newtonpolyhedron.utils.LatexConversion._

  override val varName = "x"

  //
  // UI
  //

  private lazy val inputDialog = new Dialog {
    def resetDialog(headerText: LatexString, inputPanels: Seq[Panel], eqSys: Equations[N]) = {
      ok = false
      header.content = headerText
      inputsContainer.contents.clear()
      inputsContainer.contents ++= inputPanels
      eqSysRenderer.render(eqSys, varName)
      pack()
      centerOnScreen()
      defaultButton = okBtn
    }
    var ok = false
    modal = true
    title = "Manual solver"
    private val header = new LatexRenderingComponent {
      fontSize = 30
    }
    private val inputsContainer = new FlowPanel
    private val eqSysRenderer = new EqSystemRenderingPanel
    private val okBtn = new Button("OK")
    private val cnBtn = new Button("Cancel")
    defaultButton = okBtn
    contents = new BorderPanel {
      layout(new FlowPanel {
        contents += header
      }) = North
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

  private def asEqualsZeroEquation(dim: Int) = Equation(_: Polynomial[N], EquationSign.Equals, zeroPoly(dim))

  override def getInputFor(
      system:              Polys[N],
      initialValuesOption: Option[Seq[String]],
      headerTextOption:    Option[LatexString]
  ): Option[Seq[String]] = {
    val dim = system collectFirst {
      case poly if !poly.isEmpty => poly.head.powers.length
    } getOrElse (throw new IllegalArgumentException("Can't get dimension of polys"))

    val (inputs, inputPanels) = createInputComponents(dim)
    initialValuesOption foreach { initialValues =>
      (initialValues zip inputs) map {
        case (value, input) => input.text = value
      }
    }

    inputDialog.resetDialog(
      headerText  = headerTextOption getOrElse latex(textToLatex(s"Please solve for ") + varName + ":"),
      inputPanels = inputPanels,
      eqSys       = system map asEqualsZeroEquation(dim)
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

object EqSystemSolutionDialogInput extends App {
  import org.newtonpolyhedron.entity.vector._
  import org.newtonpolyhedron.entity.equation._
  import org.newtonpolyhedron.math.internal.InternalMathProcessor
  import org.newtonpolyhedron.math.internal.Product
  import spire.implicits._
  import spire.math.Rational

  private type N = Product
  private implicit val mp: MathProcessor[N] = new InternalMathProcessor

  val s = new EqSystemSolutionDialogInput[N]

  val tricky = EqSystemRenderingPanel.tricky

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
