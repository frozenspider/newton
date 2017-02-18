package org.newtonpolyhedron.solverprinters

import java.awt.Frame
import java.io.PrintWriter
import java.text.MessageFormat

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.Surface
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.poly.PolyhedronSolver
import org.newtonpolyhedron.solve.surface.SurfaceBuilder
import org.newtonpolyhedron.ui.render3d.PolyRenderer
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PointUtils

import com.sun.j3d.utils.applet.MainFrame

import javax.vecmath.Point3d

class PolyhedronSolverPrinter(solver: PolyhedronSolver,
                              val surfaceBuilder: SurfaceBuilder,
                              val points: IndexedSeq[FracVec],
                              val commonLimitsOption: Option[IndexedSeq[IntVec]],
                              val basisOption: Option[IndexedSeq[IntVec]],
                              val illustrate: Boolean,
                              output: PrintWriter)
    extends SolverPrinter[PolyhedronSolver](solver, output) {

  override def solveFor(solver: PolyhedronSolver,
                        output: PrintWriter) = {
    output.println(title("Polyhedron computing"))

    output.println(header("Original points:"))
    val dimension = points(0).size
    for (i <- 0 until points.size) {
      output.println(MessageFormat.format(" Q{0} = {1}", int2Integer(i), points(i).toTupleString))
    }

    output.println(header("Common limits:"))
    commonLimitsOption match {
      case Some(commonLimits) =>
        for (i <- 0 until commonLimits.size) {
          output.println(MessageFormat.format(" L{0} = {1}", int2Integer(i), commonLimits(i).toTupleString))
        }
      case None =>
        output.println(" (none)")
    }

    val lookupTable = solver.solve(points, commonLimitsOption, basisOption, output)
    printLookupTable(lookupTable, output)

    val surfacesMap = surfaceBuilder.surfaces(lookupTable, dimension)

    var upperDimSurfaces = IndexedSeq.empty[Surface]
    for ((surfaceDim, currDimSurfaces) <- surfacesMap) {
      val currDimSurfacesList = currDimSurfaces.toIndexedSeq.sorted
      output.println(subheader("Surface Dimension: " + surfaceDim))
      var idx = 0
      for (surface <- currDimSurfacesList) {
        idx += 1
        output.println(MessageFormat.format("  {0})\t{1}", int2Integer(idx), surface.makeString(upperDimSurfaces)))
      }
      upperDimSurfaces = currDimSurfacesList
    }

    if (dimension <= 3 && illustrate) {
      illustrate(dimension, points, surfacesMap, 85)
    }
  }

  private def printLookupTable(lookupTable: KeyTable[IntVec, Int, Boolean],
                               output: PrintWriter) = {
    output.println(header("Compliance table:"))
    var strTable = KeyTable.empty[String, String, String]
    var rowIdx = 0
    for (rowKey <- lookupTable.rowKeys) {
      val rowKeyStr = MessageFormat.format("N{0} = {1}", int2Integer(rowIdx), rowKey.toTupleString)
      var colIdx = 0
      for (colKey <- lookupTable.colKeys) {
        val colKeyStr = MessageFormat.format(" Q{0}", int2Integer(colIdx))
        lookupTable.get(rowKey, colKey) match {
          case Some(true) => strTable = strTable + (rowKeyStr, colKeyStr, " +")
          case _          => strTable = strTable + (rowKeyStr, colKeyStr, " -")
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
                         points: Seq[FracVec],
                         surfacesMap: Map[Int, Set[Surface]],
                         freq: Int) = {
    val points3d = points map (PointUtils.p3d)
    val lines = collectLineCorners(surfacesMap(1), points)
    val borderEdgesAlt = (lines map (_.pointIndices map points3d)).flatten
    val illustrFrames = Seq(
      doDrawFrame("All-Vs-All", points3d, PolyRenderer.ALL_VS_ALL, 0, 150, 512, 512, dim == 2),
      doDrawFrame("Convex Hull", borderEdgesAlt, PolyRenderer.TRIANGLES, 512, 150, 512, 512, dim == 2)
    )
    try {
      while (!Thread.interrupted()) {
        Thread.sleep(freq)
      }
    } catch {
      case ex: InterruptedException => // NOOP
    } finally {
      illustrFrames foreach (_.dispose)
    }
  }

  private def doDrawFrame(title: String,
                          pts: Seq[Point3d],
                          mode: Int,
                          positionX: Int,
                          positionY: Int,
                          width: Int,
                          height: Int,
                          is2d: Boolean): Frame = {
    val polyRenderer = new PolyRenderer(pts, mode, is2d)
    val frame = new MainFrame(polyRenderer, width, height)
    frame.setTitle(title)
    frame.setLocation(positionX, positionY)
    frame
  }

  private def collectLineCorners(surfaces: Iterable[Surface], points: Seq[FracVec]): Seq[Surface] = {
    surfaces.map(s => if (s.size == 2) s else getLineCorners(s, points)).toSeq
  }

  private def getLineCorners(surface: Surface, points: Seq[FracVec]): Surface = {
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
    val dimension = points(0).size
    for (t <- 0 until dimension) {
      val (lesserPtIdx, greaterPtIdx) = getLesserAndGreaterPoints(surfacePtsIndices.toList, t, surfacePtsIndices(0), surfacePtsIndices(0))
      if (lesserPtIdx != greaterPtIdx)
        return new Surface(Seq(lesserPtIdx, greaterPtIdx)).addUpperSurfaces(surface.upperSurfaces)
    }
    // If this is the case - all points are same
    return new Surface(Seq(surfacePtsIndices.head)).addUpperSurfaces(surface.upperSurfaces)
  }
}
