package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import scala.collection.immutable.SortedSet

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver
import org.newtonpolyhedron.utils.LanguageImplicits._

class PolyIntersectionSolverPrinter(solver: PolyIntersectionSolver,
                                    val polyhedrons: Seq[IndexedSeq[FracVec]],
                                    val dim: Int,
                                    output: PrintWriter)
    extends SolverPrinter[PolyIntersectionSolver](solver, output) {

  override def solveFor(solver: PolyIntersectionSolver,
                        output: PrintWriter) = {
    output.println(title("Polyhedron intersection"))
    output.println(header("Original points:"))
    polyhedrons eachWithIndex { (poly, i) =>
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
