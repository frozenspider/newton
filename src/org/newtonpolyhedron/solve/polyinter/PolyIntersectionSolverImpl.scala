package org.newtonpolyhedron.solve.polyinter

import org.newtonpolyhedron._
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntVector
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.fs.utils.collection.map.BasicSortedMap
import org.fs.utils.collection.iter.AbstractIterator
import org.fs.utils.ListUtils
import org.newtonpolyhedron.utils.PointUtils
import org.newtonpolyhedron.utils.ArithUtils
import org.newtonpolyhedron.utils.NullPrintWriter
import scala.collection.IndexedSeq

class PolyIntersectionSolverImpl(val coneSolver: ConeSolver) extends PolyIntersectionSolver {

  override def solve(polyhedrons: java.util.List[java.util.List[FractionVector]],
                     dim: Int): java.util.Map[IntVector, java.util.List[java.util.List[Integer]]] = {
    val tmp = solveInner(polyhedrons map (_ map (v => fracvec2mathvec(v))), dim)
    var result = new BasicSortedMap[IntVector, java.util.List[java.util.List[Integer]]]
    for (x <- tmp.entrySet()) {
      result.put(mathvec2intvec(x.getKey()), seq2list(x.getValue() map (xs => seq2list(xs map (x => int2Integer(x))))))
    }
    result
  }

  def solveInner(polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
                 dim: Int): java.util.Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]] = {
    require(dim >= 3, "No intersections are possible below 3-dimension")
    require(polyhedrons.size >= dim - 1, "Not enough polyhedrons for intersecting at dimension " + dim)
    val polyPtsCount = polyhedrons map (_.size)

    var ptsForVectors = new BasicSortedMap[IntMathVec, IndexedSeq[IndexedSeq[Int]]]
    val polyCount = polyhedrons.size

    val initialIndices = Vector.fill(polyCount)(0).updated(polyCount - 1, -1)
    for (indices <- indicesStream(initialIndices, polyPtsCount)) {
      // ++ Construct a system
      val eqSystems = for (i <- 0 until polyCount) yield {
        PointUtils.copySubtractPointAsInt(polyhedrons(i), indices(i))
      }
      val commonEqSys = eqSystems.flatten
      // -- Construct a system

      var solutions = coneSolver.solve(seq2list(commonEqSys map (v => mathvec2intvec(v))), null, dim, NullPrintWriter.instance) map (x => intvec2mathvec(x))

      val intersectingSols = solutions filter (isIntersectingSol(eqSystems))

      for (solution <- intersectingSols) {
        var list = if (ptsForVectors.containsKey(solution))
          ptsForVectors.get(solution)
        else
          IndexedSeq.empty
        ptsForVectors.put(solution, list :+ indices)
      }
    }
    ptsForVectors
  }

  private def indicesStream(curr: IndexedSeq[Int], maximums: IndexedSeq[Int]): Stream[IndexedSeq[Int]] = {
    def calcNextRec(idx: Int, curr: IndexedSeq[Int], maximums: IndexedSeq[Int]): IndexedSeq[Int] = {
      val prev = idx - 1
      if (idx == 0 || curr(idx) < maximums(idx)) curr
      else calcNextRec(idx - 1, curr.updated(idx, 0).updated(prev, curr(prev) + 1), maximums)
    }
    val length = curr.length
    val preNextSeq = curr.updated(length - 1, curr(length - 1) + 1)
    val nextSeq = calcNextRec(length - 1, preNextSeq, maximums)
    if (nextSeq(0) < maximums(0))
      nextSeq #:: indicesStream(nextSeq, maximums)
    else Stream.empty
  }

  protected def removeNonIntersectingSolutions(solutions: java.util.List[IntVector],
                                               equationSystems: java.util.List[java.util.List[IntVector]]): java.util.List[IntVector] = {
    val eqSystemsMapped = equationSystems map (_ map (v => intvec2mathvec(v)))
    val solMapped = solutions map (v => intvec2mathvec(v))
    seq2list(solMapped.filter(isIntersectingSol(eqSystemsMapped)) map (v => mathvec2intvec(v)))
  }

  private def isIntersectingSol(equationSystems: IndexedSeq[IndexedSeq[IntMathVec]])(solution: IntMathVec): Boolean =
    equationSystems forall (_ exists (_ *+ solution == 0))
}