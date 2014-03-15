package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import org.newtonpolyhedron.entity.Matrix
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.solve.cone.ConeSolver

class ConeSolverPrinter(solver: ConeSolver,
                        val inequations: IndexedSeq[IntMathVec],
                        val basis: IndexedSeq[IntMathVec],
                        output: PrintWriter)
    extends SolverPrinter[ConeSolver](solver, output) {

  override def solveFor(solver: ConeSolver,
                        output: PrintWriter) = {
    output.println(title("Cone computing"))
    val rank = Matrix(inequations).rank
    output.println("Matrix rank = " + rank)
    output.println(header("Original inequalities:"))
    inequations eachWithIndex { (currIneq, i) =>
      output.println(MessageFormat.format(" c{0} = {1}", int2Integer(i + 1), currIneq))
    }
    val dim = inequations(0).dim
    val solved = solver.solve(inequations, basis, dim, output)
    coneFinalSolutionOutput(solved, output)
  }

  private def coneFinalSolutionOutput(testing: Seq[IntMathVec],
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