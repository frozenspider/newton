package org.newtonpolyhedron

import java.awt.Rectangle

import org.newtonpolyhedron.math.internal.InternalMathProcessor
import org.newtonpolyhedron.ui.NewtonPolyhedronFrame

/**
 * Entry point for Newton GUI.
 * There's no CLI as of now.
 *
 * @author FS
 */
object NewtonEntry {
  def main(args: Array[String]): Unit = {
    eagerAsyncInit()
    val mp = new InternalMathProcessor
    val workerLauncher = new WorkerLauncher()(mp)

    val frame = new NewtonPolyhedronFrame(workerLauncher)
    val top = frame.top
    top.bounds = new Rectangle(100, 100, 530, 550)
    top.visible = true
  }

  /** Trigger eager init for spire Numeric to avoid long ClassLoader.loadClass waiting times */
  private def eagerAsyncInit(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future
    Future {
      spire.math.Numeric
    }
  }
}
