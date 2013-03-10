package org.newtonpolyhedron.solve.polyinter

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

class PolyIntersectionSolverImpl(val coneSolver: ConeSolver) extends PolyIntersectionSolver {
  override def solve(polyhedrons: java.util.List[java.util.List[FractionVector]],
                     dim: Int): java.util.Map[IntVector, java.util.List[java.util.List[Integer]]] = {
    solveInner(polyhedrons, dim)
  }

  def solveInner(polyhedrons: java.util.List[java.util.List[FractionVector]],
                 dim: Int): java.util.Map[IntVector, java.util.List[java.util.List[Integer]]] = {
    //  def solveInner(polyhedrons: IndexedSeq[IndexedSeq[FracMathVec]],
    //                 dim: Int): Map[IntMathVec, IndexedSeq[IndexedSeq[Int]]] = {
    require(dim >= 3, "No intersections are possible below 3-dimension")
    require(polyhedrons.size >= dim - 1, "Not enough polyhedrons for intersecting at dimension " + dim)
    var polyPtsCount = new java.util.ArrayList[Integer]();
    for (poly <- polyhedrons) {
      polyPtsCount.add(poly.size)
    }

    var ptsForVectors = new BasicSortedMap[IntVector, java.util.List[java.util.List[Integer]]]
    val polyCount = polyhedrons.size

    val iter = new IndicesIterable(polyCount, polyPtsCount).iterator
    while (iter.hasNext) {
      val indices = iter.next

      // ++ Construct a system
      var eqSystems = new java.util.ArrayList[java.util.List[IntVector]]
      for (i <- 0 until polyCount) {
        eqSystems.add(PointUtils.copySubtractPointAsInt(polyhedrons.get(i), indices.get(i)));
      }
      var commonEqSys = new java.util.ArrayList[IntVector]
      for (eqSys <- eqSystems) {
        commonEqSys.addAll(eqSys);
      }
      // -- Construct a system

      var solution = new java.util.ArrayList[IntVector](coneSolver.solve(commonEqSys, null, dim, NullPrintWriter.instance));

      removeNonIntersectingSolutions(solution, eqSystems);

      for (solutionVector <- solution) {
        var list = ptsForVectors.get(solutionVector);
        if (list == null) {
          list = new ArrayList[java.util.List[Integer]]
          ptsForVectors.put(solutionVector, list);
        }
        list.add(indices);
      }
    }
    ptsForVectors
  }

  protected def removeNonIntersectingSolutions(solution: java.util.List[IntVector],
                                               equationSystems: java.util.List[java.util.List[IntVector]]): Unit = {
    var solutionIter = solution.iterator
    while (solutionIter.hasNext) {
      val currSolution = solutionIter.next()
      var removed = false
      for (currEqSystem <- equationSystems if !removed) {
        var nullifiesAtLeastOne = false
        for (eq <- currEqSystem) {
          if (ArithUtils.isZero(eq.dotProduct(currSolution))) {
            nullifiesAtLeastOne = true;
          }
        }
        if (!nullifiesAtLeastOne) {
          solutionIter.remove
          removed = true;
        }
      }
    }
  }
}

//
// Inner classes
//
private sealed class IndicesIterable(val length: Int, val maximums: java.util.List[Integer])
    extends java.lang.Iterable[java.util.List[Integer]] {

  override def iterator: IndicesIterator =
    new IndicesIterator(length, maximums)
}

private sealed class IndicesIterator extends org.fs.utils.collection.iter.AbstractIterator[java.util.List[Integer]] {

  private var length: Int = _
  private var current: java.util.List[Integer] = _
  private var nextV: java.util.List[Integer] = _
  private var maximums: java.util.List[Integer] = _

  def this(length: Int, maximums: java.util.List[Integer]) = {
    this()
    this.length = length
    this.current = new ArrayList[Integer](
      Collections.nCopies(
        length,
        Integer.valueOf(0)));
    this.current.set(length - 1, -1);
    this.maximums = maximums
  }

  override def hasNext: Boolean = this.synchronized {
    if (nextV != null) return true;
    doGetNext
    return nextV != null;
  }

  override def next: java.util.List[Integer] = this.synchronized {
    if (!hasNext()) throw new NoSuchElementException();
    current = nextV;
    nextV = null;
    return new ArrayList[Integer](current);
  }

  private def doGetNext: Unit = {
    nextV = new ArrayList[Integer](current);
    var idx = nextV.size() - 1;
    ListUtils.inc(nextV, idx);
    var over = false
    while (!over && idx > 0) {
      if (nextV.get(idx) < maximums.get(idx)) {
        over = true
      } else {
        nextV.set(idx, 0);
        idx -= 1
        ListUtils.inc(nextV, idx);
      }
    }
    if (nextV.get(0) >= maximums.get(0)) {
      nextV = null;
    }
  }
}