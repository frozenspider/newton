package org.newtonpolyhedron.solverprinters

import java.io.PrintWriter
import java.text.MessageFormat

import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver

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
    import org.newtonpolyhedron.utils.ScalaJavaConversionUtils._
    var strTable = new ArrayListKeyTable[Int, IntVec, String]
    for {
      r <- vectorPointTable.rowKeyList()
      c <- vectorPointTable.colKeyList()
    } {
      strTable.put(r, c, vectorPointTable.get(r, c).mkString("(Q", ", Q", ")"))
    }
    output.println(strTable)
  }
}