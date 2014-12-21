package org.newtonpolyhedron.solve.polyinter

import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.NullPrintWriter
import org.newtonpolyhedron.utils.PointUtils

class PolyIntersectionSolverImpl(val coneSolver: ConeSolver) extends PolyIntersectionSolver {

  override def solve(polyhedrons: Seq[IndexedSeq[FracVec]],
                     dim: Int): KeyTable[Int, IntVec, SortedSet[Int]] = {
    require(dim >= 3, "No intersections are possible below 3-dimension")
    require(polyhedrons.size >= dim - 1, "Not enough polyhedrons for intersecting at dimension " + dim)
    val polySizes = polyhedrons.map(_.size).toIndexedSeq
    val initialIndices = IndexedSeq.fill(polyhedrons.size)(0).updated(polyhedrons.size - 1, -1)
    val indices = indicesStream(initialIndices, polySizes)
    val ptsForVectors = fillPtsForVectorsMap(indices, polyhedrons, dim)
    val ptsForPolysAndVectors = reverseTableMeaning(ptsForVectors)
    ptsForPolysAndVectors
  }

  /** @return { vector : [ point indices list per polyhedron ] } */
  def fillPtsForVectorsMap(indicesSeq: Seq[IndexedSeq[Int]],
                           polyhedrons: Seq[IndexedSeq[FracVec]],
                           dim: Int): Map[IntVec, Seq[IndexedSeq[Int]]] = {
    def fillMapRecursive(indicesSeq: Seq[IndexedSeq[Int]],
                         prevMap: Map[IntVec, Seq[IndexedSeq[Int]]]): Map[IntVec, Seq[IndexedSeq[Int]]] =
      if (indicesSeq.isEmpty) prevMap
      else {
        val indices = indicesSeq.head
        // ++ Construct a system
        val eqSystems = polyhedrons mapWithIndex { (poly, i) =>
          val idx = indices(i)
          PointUtils.copySubtractPointAsInt(poly, idx)
        }
        val commonEqSys = eqSystems.flatten
        // -- Construct a system

        val solutions = coneSolver solve (commonEqSys, None, dim, NullPrintWriter)
        val intersectingSols = solutions filter (isIntersectingSol(eqSystems))
        fillMapRecursive(indicesSeq.tail, withSoluionsToIndices(prevMap, intersectingSols, indices))
      }
    fillMapRecursive(indicesSeq, Map.empty)
  }

  def withSoluionsToIndices(prevMap: Map[IntVec, Seq[IndexedSeq[Int]]],
                            solutions: Seq[IntVec],
                            indices: IndexedSeq[Int]): Map[IntVec, Seq[IndexedSeq[Int]]] =
    solutions.headOption match {
      case None => prevMap
      case Some(sol) => {
        val nextMap = if (prevMap contains sol)
          prevMap + (sol -> (prevMap(sol) :+ indices))
        else
          prevMap + (sol -> IndexedSeq(indices))
        withSoluionsToIndices(nextMap, solutions.tail, indices)
      }
    }

  def indicesStream(curr: IndexedSeq[Int],
                    maximums: IndexedSeq[Int]): Stream[IndexedSeq[Int]] = {
    def calcNextRec(idx: Int,
                    curr: IndexedSeq[Int],
                    maximums: IndexedSeq[Int]): IndexedSeq[Int] = {
      val prev = idx - 1
      if (idx == 0 || curr(idx) < maximums(idx)) curr
      else calcNextRec(idx - 1, curr.updated(idx, 0).updated(prev, curr(prev) + 1), maximums)
    }
    val len = curr.length
    val preNextSeq = curr updated (len - 1, curr.last + 1)
    val nextSeq = calcNextRec(len - 1, preNextSeq, maximums)
    if (nextSeq(0) < maximums(0))
      nextSeq #:: indicesStream(nextSeq, maximums)
    else Stream.empty
  }

  def isIntersectingSol(equationSystems: Seq[Seq[IntVec]])(solution: IntVec): Boolean =
    equationSystems forall (_ exists (_ *+ solution == 0))

  /**
   * @param ptsForVectors { vector : [ point indices list per polyhedron ] }
   * @return { polyIdx, vector -> [ points giving this vector for this poly when intersecting ] }
   */
  def reverseTableMeaning(ptsForVectors: Map[IntVec, Seq[IndexedSeq[Int]]]): KeyTable[Int, IntVec, SortedSet[Int]] = {
    import org.newtonpolyhedron.utils.ScalaJavaConversionUtils._
    var vectPtTable = new ArrayListKeyTable[Int, IntVec, SortedSet[Int]]
    for ((vector, indicesSeq) <- ptsForVectors) {
      for (indices <- indicesSeq) {
        for (i <- 0 until indices.size) {
          val pts = vectPtTable.get(i, vector, SortedSet.empty)
          vectPtTable.put(i, vector, pts + indices(i))
        }
      }
      // Sanity check that all combinations are present
      // I.e. [0, 5], [0, 6], [0, 7], [1, 5], [1, 6], [1, 7] must all present to get [0, 1], [5, 6, 7]
      // We'll use a weaker assertion: number of combinations must be equal to product of resulting point set sizes
      assert(vectPtTable.getCol(vector).values.map(_.size).product == indicesSeq.size, "Logic failure, please report to developer")
    }
    KeyTables.sortByColHeaders(vectPtTable, intVecOrdering, true)
    vectPtTable
  }
}
