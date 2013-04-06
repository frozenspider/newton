package org.newtonpolyhedron.solverprinters
import java.awt.Frame
import java.io.PrintWriter
import java.text.MessageFormat

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.newtonpolyhedron._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.vector.IntVector
import org.newtonpolyhedron.solve.poly.PolyhedronSolver
import org.newtonpolyhedron.solve.surface.SurfaceBuilder
import org.newtonpolyhedron.ui.render3d.PolyRenderer
import org.newtonpolyhedron.utils.PointUtils

import com.sun.j3d.utils.applet.MainFrame

import javax.vecmath.Point3d

class PolyhedronSolverPrinter(solver: PolyhedronSolver,
                              val surfaceBuilder: SurfaceBuilder,
                              val points: IndexedSeq[FracMathVec],
                              val commonLimits: IndexedSeq[IntMathVec],
                              val basis: IndexedSeq[IntMathVec],
                              val illustrate: Boolean,
                              output: PrintWriter)
    extends SolverPrinter[PolyhedronSolver](solver, output) {

  def this(solver: PolyhedronSolver,
           surfaceBuilder: SurfaceBuilder,
           points: java.util.List[FractionVector],
           commonLimits: java.util.List[IntVector],
           basis: java.util.List[IntVector],
           illustrate: Boolean,
           output: PrintWriter) =
    this(solver, surfaceBuilder,
      points map fracvec2mathvec,
      commonLimits map intvec2mathvec,
      basis map intvec2mathvec,
      illustrate, output)

  override def solveFor(solver: PolyhedronSolver,
                        output: PrintWriter) = {
    output.println(title("Polyhedron computing"))

    output.println(header("Original points:"))
    val dim = points(0).dim
    for (i <- 0 until points.size) {
      output.println(MessageFormat.format(" Q{0} = {1}", int2Integer(i), points(i)))
    }

    output.println(header("Common limits:"))
    if (commonLimits.size > 0) {
      for (i <- 0 until commonLimits.size) {
        output.println(MessageFormat.format(" L{0} = {1}", int2Integer(i), commonLimits(i)))
      }
    } else {
      output.println(" (none)")
    }

    val lookupTable = solver.solve(points, commonLimits, basis, output)
    printLookupTable(lookupTable, output)

    val surfacesMap = surfaceBuilder.surfaces(lookupTable, dim)

    var upperDimSurfaces = List.empty[Surface]
    for ((surfaceDim, currDimSurfaces) <- surfacesMap) {
      val currDimSurfacesList = currDimSurfaces.toList.sorted
      output.println(subheader("Surface Dimension: " + surfaceDim))
      var idx = 0
      for (surface <- currDimSurfacesList) {
        idx += 1
        output.println(MessageFormat.format("  {0})\t{1}", int2Integer(idx), surface.makeString(seq2list(upperDimSurfaces))))
      }
      upperDimSurfaces = currDimSurfacesList
    }

    if (dim <= 3 && illustrate) {
      illustrate(dim, points, surfacesMap, 85)
    }
  }

  private def printLookupTable(lookupTable: KeyTable[IntMathVec, Int, Boolean],
                               output: PrintWriter) = {
    var strTable = new ArrayListKeyTable[String, String, String]
    var rowIdx = 0
    for (rowKey <- lookupTable.rowKeyList) {
      val rowKeyStr = MessageFormat.format("N{0} = {1}", int2Integer(rowIdx), rowKey)
      var colIdx = 0
      for (colKey <- lookupTable.colKeyList) {
        val colKeyStr = MessageFormat.format(" Q{0}", int2Integer(colIdx))
        val value = lookupTable.get(rowKey, colKey)
        if (value) {
          strTable.put(rowKeyStr, colKeyStr, " +")
        } else {
          strTable.put(rowKeyStr, colKeyStr, " -")
        }
        colIdx += 1
      }
      rowIdx += 1
    }
    // Skip "+-----+----+" lines
    val lines = strTable.toString.split("\n")
    for (line <- lines if !(line startsWith "+-")) {
      output.println(line)
    }
  }

  private def illustrate(dim: Int,
                         points: Seq[FracMathVec],
                         surfacesMap: Map[Int, Set[Surface]],
                         freq: Int) = {
    val points3d = points map (PointUtils.toPoint3d)
    val lines = collectLineCorners(surfacesMap(1), points)
    val borderEdgesAlt = (lines map (_.pointIndices map points3d)).flatten
    val illustrFrames = Seq(
      doDrawFrame(points3d, PolyRenderer.ALL_VS_ALL, 0, 150, 512, 512, dim == 2),
      doDrawFrame(borderEdgesAlt, PolyRenderer.TRIANGLES, 512, 150, 512, 512, dim == 2))
    try {
      while (!Thread.interrupted()) {
        Thread.sleep(freq)
      }
    } catch {
      case ex: InterruptedException => // NOOP
    } finally {
      illustrFrames map (_.dispose)
    }
  }

  private def doDrawFrame(pts: Seq[Point3d],
                          mode: Int,
                          positionX: Int,
                          positionY: Int,
                          width: Int,
                          height: Int,
                          is2d: Boolean): Frame = {
    val polyRenderer = new PolyRenderer(seq2list(pts), mode, is2d)
    val frame = new MainFrame(polyRenderer, width, height)
    frame.setLocation(positionX, positionY)
    frame
  }

  private def collectLineCorners(surfaces: Iterable[Surface], points: Seq[FracMathVec]): Seq[Surface] = {
    surfaces.map(s => if (s.size == 2) s else getLineCorners(s, points)).toSeq
  }

  private def getLineCorners(surface: Surface, points: Seq[FracMathVec]): Surface = {
    def getLesserAndGreaterPoints(surfacePtsIndices: List[Int],
                                  currDim: Int, lesserPtIdx: Int, greaterPtIdx: Int): (Int, Int) =
      surfacePtsIndices match {
        case Nil => (lesserPtIdx, greaterPtIdx)
        case x :: xs => {
          val newLesser =
            if (points(x)(currDim) < points(lesserPtIdx)(currDim)) x
            else lesserPtIdx
          val newGreater =
            if (points(x)(currDim) > points(greaterPtIdx)(currDim)) x
            else greaterPtIdx
          getLesserAndGreaterPoints(xs, currDim, newLesser, newGreater)
        }
      }
    val surfacePtsIndices = surface.pointIndices.toIndexedSeq
    val dim = points(0).dim
    for (t <- 0 until dim) {
      val (lesserPtIdx, greaterPtIdx) = getLesserAndGreaterPoints(surfacePtsIndices.toList, t, surfacePtsIndices(0), surfacePtsIndices(0))
      if (lesserPtIdx != greaterPtIdx)
        return new Surface(Seq(lesserPtIdx, greaterPtIdx)).addUpperSurfaces(surface.upperSurfaces)
    }
    // If this is the case - all points are same
    return new Surface(Seq(surfacePtsIndices.head)).addUpperSurfaces(surface.upperSurfaces)
  }
}
