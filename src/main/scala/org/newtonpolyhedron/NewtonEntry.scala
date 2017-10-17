package org.newtonpolyhedron

import java.awt.Rectangle

import org.newtonpolyhedron.ui.NewtonPolyhedronFrame
import org.newtonpolyhedron.math.internal.InternalMathProcessor

object NewtonEntry {
  def main(args: Array[String]): Unit = {
    val mp = new InternalMathProcessor
    val logic = new NewtonLogic()(mp)

    val frame = new NewtonPolyhedronFrame(logic)
    val top = frame.top
    top.bounds = new Rectangle(100, 100, 530, 550)
    top.visible = true
  }
}
