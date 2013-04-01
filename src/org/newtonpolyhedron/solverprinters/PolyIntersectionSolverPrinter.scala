package org.newtonpolyhedron.solverprinters
import org.newtonpolyhedron._
import java.io.PrintWriter
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver
import java.text.MessageFormat
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.fs.utils.collection.table.ArrayListKeyTable
import org.newtonpolyhedron.entity.vector.FractionVector
import org.fs.utils.collection.table.KeyTables
import scala.collection.immutable.SortedSet

class PolyIntersectionSolverPrinter(solver: PolyIntersectionSolver,
                                    val polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
                                    val dim: Int,
                                    output: PrintWriter)
    extends SolverPrinter[PolyIntersectionSolver](solver, output) {

  def this(solver: PolyIntersectionSolver,
           polyhedrons: java.util.List[java.util.List[FractionVector]],
           dim: Int,
           output: PrintWriter) =
    this(solver, polyhedrons map (_ map fracvec2mathvec), dim, output)

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
    val ptsForVectors = solver.solve(polyhedrons, dim)

    val vectorPointTable = reverseTableMeaning(ptsForVectors)
    output.println(vectorPointTable) // TODO: print without TreeSet(...)
  }

  private def reverseTableMeaning(ptsForVectors: Map[IntMathVec, Seq[IndexedSeq[Int]]]): KeyTable[Int, IntMathVec, SortedSet[Int]] = {
    var vectPtTable = new ArrayListKeyTable[Int, IntMathVec, SortedSet[Int]]
    for ((vector, indicesSeq) <- ptsForVectors) {
      for (indices <- indicesSeq) {
        for (i <- 0 until indices.size) {
          val pts = vectPtTable.get(i, vector, SortedSet.empty)
          vectPtTable.put(i, vector, pts + indices(i))
        }
      }
    }
    KeyTables.sortByColHeaders(vectPtTable, true)
    vectPtTable
  }
}