package org.newtonpolyhedron

import java.awt.Rectangle

import org.newtonpolyhedron.ui.NewtonPolyhedronFrame

object _NewtonEntryScala {
  def main(args: Array[String]): Unit = {
    val frame = new NewtonPolyhedronFrame
    val top = frame.top
    top.bounds = new Rectangle(100, 100, 530, 550)
    top.visible = true
  }
}