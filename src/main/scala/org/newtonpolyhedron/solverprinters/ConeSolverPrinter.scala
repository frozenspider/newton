package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import org.newtonpolyhedron.NewtonImports._

import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.cone.ConeSolver

class ConeSolverPrinter[N <: MPNumber, M <: MPMatrix](
  override val solver: ConeSolver,
  val inequations:     IndexedSeq[IntVec],
  val basisOption:     Option[IndexedSeq[IntVec]],
  override val output: PrintWriter
)(implicit mp: MathProcessor[N, M])
    extends SolverPrinter[ConeSolver](solver, output) {

  override def solveFor(
      solver: ConeSolver,
      output: PrintWriter
  ) = {
    output.println(title("Cone computation"))
    val rank = mp.matrix(inequations map (_ map mp.fromBigInt)).rank
    output.println("Matrix rank = " + rank)
    output.println(header("Original inequalities:"))
    inequations foreachWithIndex { (currIneq, i) =>
      output.println(MessageFormat.format(" c{0} = {1}", int2Integer(i + 1), currIneq.toTupleString))
    }
    val dimension = inequations(0).size
    val solved = solver.solve(inequations, basisOption, dimension)
    coneFinalSolutionOutput(solved, output)
  }

  private def coneFinalSolutionOutput(
      testing: Seq[IntVec],
      output:  PrintWriter
  ) = {
    output.println(header("FINAL SOLUTIONS:"))
    testing foreachWithIndex { (currTesting, i) =>
      val str = currTesting.toTupleString
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
    testing map (_.toTupleString) foreach (output.println)
  }
}
