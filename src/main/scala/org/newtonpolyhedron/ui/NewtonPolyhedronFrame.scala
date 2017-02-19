package org.newtonpolyhedron.ui

import java.awt.Font
import java.io.File
import java.io.FileNotFoundException
import java.io.PrintWriter
import java.io.Writer

import scala.swing._
import scala.swing.event.ButtonClicked

import org.newtonpolyhedron.NewtonLogic
import org.newtonpolyhedron.WorkingMode
import org.newtonpolyhedron.ex.WrongFormatException
import org.newtonpolyhedron.BuildInfo

class NewtonPolyhedronFrame extends SimpleSwingApplication {

  val printWriter = new PrintWriter(new NewtonTextAreaOutput)
  val logic = new NewtonLogic
  var workingThread: Option[Thread] = None

  def top = new MainFrame {
    title = s"Newton ${BuildInfo.version}"
    contents = ui
  }

  val ui = new BorderPanel {
    import BorderPanel.Position._
    val browseBtn = new Button("Browse")
    listenTo(startBtn, browseBtn)

    layout(new BorderPanel {
      layout(new FlowPanel(FlowPanel.Alignment.Left)(startBtn, chckbxIllustrate)) = North
      layout(new FlowPanel(FlowPanel.Alignment.Left)(cbMode)) = Center
      layout(new BorderPanel {
        layout(browseBtn) = West
        layout(tfPathToInput) = Center
      }) = South
    }) = North

    layout(new BorderPanel {
      layout(new ScrollPane(txtrOutput)) = Center
    }) = Center

    reactions += {
      case ButtonClicked(`startBtn`)  => eventStartClick
      case ButtonClicked(`browseBtn`) => eventBrowseClick
    }
  }

  lazy val tfPathToInput = new TextField(10)
  lazy val chckbxIllustrate = new CheckBox("Illustrate if possible")
  lazy val cbMode = new ComboBox(WorkingMode.values.toSeq)
  lazy val txtrOutput = new TextArea {
    font = new Font("Courier New", Font.PLAIN, 12)
    text = "Press Browse button to browse for input files and press Start to begin.\n"
  }
  lazy val startBtn = new Button("Start")

  def eventStartClick: Unit = {
    try {
      workingThread match {
        case None => {
          val path = tfPathToInput.text
          val selectedIdx = cbMode.selection.index
          val illustrate = chckbxIllustrate.selected
          val mode = cbMode.selection.item
          if (selectedIdx != -1) {
            workingThread = Some(logic.makeThread(path, mode, illustrate, printWriter))
            workingThread map (_.start)
            startBtn.text = "Stop"
          }
        }
        case Some(wt) => {
          wt.interrupt
          workingThread = None
          startBtn.text = "Start"
        }
      }
    } catch {
      case ex: FileNotFoundException => printWriter.println("File not found")
      case ex: WrongFormatException  => printWriter.println("Illegal file format, see instruction")
      case ex: Throwable             => ex.printStackTrace(printWriter)
    }

  }

  def eventBrowseClick: Unit = {
    val chooser = new FileChooser {
      val dir =
        if (tfPathToInput.text.isEmpty) {
          new File(".")
        } else {
          new File(tfPathToInput.text)
        }
      peer.setCurrentDirectory(dir)
    }
    val returnVal = chooser.showOpenDialog(ui)
    returnVal match {
      case FileChooser.Result.Approve => tfPathToInput.text = chooser.selectedFile.getAbsolutePath
      case _                          => // NOOP
    }
  }

  class NewtonTextAreaOutput extends Writer {
    override def write(cbuf: Array[Char], off: Int, len: Int) = writeImpl(new String(cbuf, off, len))
    override def flush = {}
    override def close = {}
    private def writeImpl(text: String) {
      print(text)
      txtrOutput append text
      txtrOutput.caret.position = txtrOutput.text.length
    }
  }
}
