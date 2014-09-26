package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.MatrixSupport
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.cone.ConeSolver

class ConeSolverPrinter(solver: ConeSolver,
                        val inequations: IndexedSeq[IntVec],
                        val basis: IndexedSeq[IntVec],
                        output: PrintWriter)
    extends SolverPrinter[ConeSolver](solver, output) {

  override def solveFor(solver: ConeSolver,
                        output: PrintWriter) = {
    output.println(title("Cone computing"))
    val rank = MatrixSupport.fromInts(inequations).rank
    output.println("Matrix rank = " + rank)
    output.println(header("Original inequalities:"))
    inequations eachWithIndex { (currIneq, i) =>
      output.println(MessageFormat.format(" c{0} = {1}", int2Integer(i + 1), currIneq))
    }
    val dimension = inequations(0).size
    val solved = solver.solve(inequations, basis, dimension, output)
    coneFinalSolutionOutput(solved, output)
  }

  private def coneFinalSolutionOutput(testing: Seq[IntVec],
                                      output: PrintWriter) = {
    output.println(header("FINAL SOLUTIONS:"))
    testing eachWithIndex { (currTesting, i) =>
      val str = currTesting.toString
      val len = str.length
      output.print(str + "\t")
      if (len < 10) {
        output.print("\t")
      }
      if (len < 16) {
        output.print("\t")
      }
      if ((i + 1) % 4 == 0) {
        output.println()
      }
    }
    output.println(header("ANSWERS:"))
    testing foreach (output.println)
  }
}