package org.newtonpolyhedron.solve.polyinter

import org.newtonpolyhedron._
import org.newtonpolyhedron.solve.cone.ConeSolver
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntVector
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.fs.utils.collection.map.BasicSortedMap
import org.fs.utils.collection.iter.AbstractIterator
import java.util.ArrayList
import java.util.Collections
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

    val iter = new IndicesIterable(polyCount, polyPtsCount).iterator
    while (iter.hasNext) {
      val indices = iter.next

      // ++ Construct a system
      val eqSystems = for (i <- 0 until polyCount) yield {
        PointUtils.copySubtractPointAsInt(polyhedrons(i), indices(i))
      }
      val commonEqSys = eqSystems.flatten
      // -- Construct a system

      var solutions = coneSolver.solve(seq2list(commonEqSys map (v => mathvec2intvec(v))), null, dim, NullPrintWriter.instance) map (x => intvec2mathvec(x))

      val intersectingSols = solutions filter (isIntersectingSol(eqSystems))

      for (solution <- intersectingSols) {
        var list = ptsForVectors.get(solution);
        if (list == null) {
          list = IndexedSeq.empty
        }
        ptsForVectors.put(solution, list :+ indices)
      }
    }
    ptsForVectors
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

//
// Inner classes
//
private sealed class IndicesIterable(val length: Int, val maximums: IndexedSeq[Int])
    extends java.lang.Iterable[IndexedSeq[Int]] {

  override def iterator: IndicesIterator =
    new IndicesIterator(length, maximums)
}

private sealed class IndicesIterator extends org.fs.utils.collection.iter.AbstractIterator[IndexedSeq[Int]] {

  private var length: Int = _
  private var current: java.util.List[Int] = _
  private var nextV: java.util.List[Int] = _
  private var maximums: java.util.List[Int] = _

  def this(length: Int, maximums: IndexedSeq[Int]) = {
    this()
    this.length = length
    this.current = new ArrayList[Int](seq2list(Vector.fill(length)(0)));
    this.current.set(length - 1, -1);
    this.maximums = new ArrayList[Int](seq2list(maximums))
  }

  override def hasNext: Boolean = this.synchronized {
    if (nextV != null) return true;
    doGetNext
    return nextV != null;
  }

  override def next: IndexedSeq[Int] = this.synchronized {
    if (!hasNext()) throw new NoSuchElementException();
    current = nextV;
    nextV = null;
    return new ArrayList(current);
  }

  private def doGetNext: Unit = {
    nextV = new ArrayList(current);
    var idx = nextV.size() - 1;
    nextV.set(idx, nextV(idx) + 1)
    var over = false
    while (!over && idx > 0) {
      if (nextV.get(idx) < maximums.get(idx)) {
        over = true
      } else {
        nextV.set(idx, 0);
        idx -= 1
        nextV.set(idx, nextV(idx) + 1)
      }
    }
    if (nextV.get(0) >= maximums.get(0)) {
      nextV = null;
    }
  }
}
