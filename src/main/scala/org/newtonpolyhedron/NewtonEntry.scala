package org.newtonpolyhedron

import java.awt.Rectangle

import org.newtonpolyhedron.ui.NewtonPolyhedronFrame
import org.newtonpolyhedron.math.internal.InternalMathProcessor

object NewtonEntry {
  def main(args: Array[String]): Unit = {
    eagerAsyncInit()
    val mp = new InternalMathProcessor
    val logic = new NewtonLogic()(mp)

    val frame = new NewtonPolyhedronFrame(logic)
    val top = frame.top
    top.bounds = new Rectangle(100, 100, 530, 550)
    top.visible = true
  }

  /** Trigger eager init for spire Numeric to avoid long ClassLoader.loadClass waiting times */
  private def eagerAsyncInit(): Unit = {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global
    Future {
      spire.math.Numeric
    }
  }
}
