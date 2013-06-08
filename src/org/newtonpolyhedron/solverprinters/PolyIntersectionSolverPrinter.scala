package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver

class PolyIntersectionSolverPrinter(solver: PolyIntersectionSolver,
                                    val polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
                                    val dim: Int,
                                    output: PrintWriter)
    extends SolverPrinter[PolyIntersectionSolver](solver, output) {

  override def solveFor(solver: PolyIntersectionSolver,
                        output: PrintWriter) = {
    output.println(title("Polyhedron intersection"))
    output.println(header("Original points:"))
    for (i <- 0 until polyhedrons.size) {
      output.println(subheader("Poly " + i))
      val poly = polyhedrons(i)
      for (j <- 0 until poly.size) {
        output.println(MessageFormat.format(" Q{0} = {1}", int2Integer(j), poly(j)))
      }
    }
    val vectorPointTable = solver.solve(polyhedrons, dim)

    printTable(vectorPointTable, output)
  }

  private def printTable(vectorPointTable: KeyTable[Int, IntMathVec, SortedSet[Int]], output: PrintWriter): Unit = {
    var strTable = new ArrayListKeyTable[Int, IntMathVec, String]
    for {
      r <- vectorPointTable.rowKeyList()
      c <- vectorPointTable.colKeyList()
    } {
      strTable.put(r, c, vectorPointTable.get(r, c).mkString("(Q", ", Q", ")"))
    }
    output.println(strTable)
  }
}