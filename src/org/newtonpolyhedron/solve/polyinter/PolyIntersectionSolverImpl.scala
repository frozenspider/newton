package org.newtonpolyhedron.solve.polyinter

import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntVector
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.utils.NullPrintWriter
import org.newtonpolyhedron.utils.PointUtils

class PolyIntersectionSolverImpl(val coneSolver: ConeSolver) extends PolyIntersectionSolver {

  override def solve(polyhedrons: java.util.List[java.util.List[FractionVector]],
                     dim: Int): java.util.Map[IntVector, java.util.List[java.util.List[Integer]]] = {
    val tmp = solveInner(polyhedrons map (_ map (v => fracvec2mathvec(v))), dim)
    var result = new org.fs.utils.collection.map.BasicSortedMap[IntVector, java.util.List[java.util.List[Integer]]]
    for ((k, v) <- tmp) {
      result.put(mathvec2intvec(k), seq2list(v map (xs => seq2list(xs map (x => int2Integer(x))))))
    }
    result
  }

  def solveInner(polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
                 dim: Int): Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]] = {
    require(dim >= 3, "No intersections are possible below 3-dimension")
    require(polyhedrons.size >= dim - 1, "Not enough polyhedrons for intersecting at dimension " + dim)
    val polySizes = polyhedrons map (_.size)
    val initialIndices = IndexedSeq.fill(polyhedrons.size)(0).updated(polyhedrons.size - 1, -1)
    val indices = indicesStream(initialIndices, polySizes)
    val ptsForVectors = fillPtsForVectorsMap(indices, polyhedrons, dim)
    ptsForVectors
  }

  def fillPtsForVectorsMap(indicesSeq: Seq[IndexedSeq[Int]],
                           polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
                           dim: Int): Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]] = {
    def fillMapRecursive(indicesSeq: Seq[IndexedSeq[Int]],
                         prevMap: Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]]): Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]] =
      if (indicesSeq.isEmpty) prevMap
      else {
        val indices = indicesSeq.head
        // ++ Construct a system
        val eqSystems =
          for {
            i <- 0 until polyhedrons.size
            val poly = polyhedrons(i)
            val idx = indices(i)
          } yield PointUtils.copySubtractPointAsInt(poly, idx)
        val commonEqSys = eqSystems.flatten
        // -- Construct a system

        val solutions = coneSolver solve (commonEqSys, IndexedSeq.empty, dim, NullPrintWriter.instance)
        val intersectingSols = solutions filter (isIntersectingSol(eqSystems))
        fillMapRecursive(indicesSeq.tail, withSoluionsToIndices(prevMap, intersectingSols, indices))
      }
    fillMapRecursive(indicesSeq, Map.empty)
  }

  def withSoluionsToIndices(prevMap: Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]],
                            solutions: IndexedSeq[IntMathVec],
                            indices: IndexedSeq[Int]): Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]] =
    solutions.headOption match {
      case None => prevMap
      case Some(sol) => {
        val nextMap = if (prevMap contains (sol))
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
    val preNextSeq = curr.updated(len - 1, curr(len - 1) + 1)
    val nextSeq = calcNextRec(len - 1, preNextSeq, maximums)
    if (nextSeq(0) < maximums(0))
      nextSeq #:: indicesStream(nextSeq, maximums)
    else Stream.empty
  }

  def removeNonIntersectingSolutions(solutions: java.util.List[IntVector],
                                     equationSystems: java.util.List[java.util.List[IntVector]]): java.util.List[IntVector] = {
    val eqSystemsMapped = equationSystems map (_ map (v => intvec2mathvec(v)))
    val solMapped = solutions map (v => intvec2mathvec(v))
    seq2list(solMapped.filter(isIntersectingSol(eqSystemsMapped)) map (v => mathvec2intvec(v)))
  }

  def isIntersectingSol(equationSystems: IndexedSeq[IndexedSeq[IntMathVec]])(solution: IntMathVec): Boolean =
    equationSystems forall (_ exists (_ *+ solution == 0))
}