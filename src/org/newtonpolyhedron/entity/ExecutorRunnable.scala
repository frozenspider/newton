package org.newtonpolyhedron.entity

import java.io.PrintWriter

class ExecutorRunnable(solver: SolverPrinter[_], output: PrintWriter) extends Runnable {

  override def run: Unit =
    try
      solver.solveAndPrint
    catch {
      case th: Throwable => th.printStackTrace(output)
    }
}