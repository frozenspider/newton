package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import scala.collection.immutable.SortedSet

import org.newtonpolyhedron.NewtonImports._
import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver

class PolyIntersectionSolverPrinter[N <: MPNumber](
  override val solver: PolyIntersectionSolver[N],
  val polyhedrons:     Seq[IndexedSeq[NumVec[N]]],
  val dim:             Int,
  override val output: PrintWriter
)
    extends SolverPrinter[PolyIntersectionSolver[N]](solver, output) {

  override def solveFor(
      solver: PolyIntersectionSolver[N],
      output: PrintWriter
  ) = {
    output.println(title("Polyhedron intersection"))
    output.println(header("Original points:"))
    polyhedrons foreachWithIndex { (poly, i) =>
      output.println(subheader("Poly " + i))
      for (j <- 0 until poly.size) {
        output.println(MessageFormat.format(" Q{0} = {1}", int2Integer(j), poly(j)))
      }
    }
    val vectorPointTable = solver.solve(polyhedrons, dim)

    printTable(vectorPointTable, output)
  }

  private def printTable(vectorPointTable: KeyTable[Int, IntVec, SortedSet[Int]], output: PrintWriter): Unit = {
    val strTable = vectorPointTable map (_ mkString ("(Q", ", Q", ")"))
    output.println(strTable)
  }
}
