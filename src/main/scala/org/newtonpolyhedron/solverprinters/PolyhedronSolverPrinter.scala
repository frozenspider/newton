package org.newtonpolyhedron.solverprinters

import java.awt.Frame
import java.io.PrintWriter
import java.text.MessageFormat

import org.fs.utility.collection.table.KeyTable
import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.Face
import org.newtonpolyhedron.solve.poly.PolyhedronSolver
import org.newtonpolyhedron.solve.face.FaceBuilder
import org.newtonpolyhedron.ui.render3d.PolyRenderer
import org.newtonpolyhedron.utils.PointUtils

import com.sun.j3d.utils.applet.MainFrame

import javax.vecmath.Point3d

class PolyhedronSolverPrinter[N <: MPNumber](
  solver:                 PolyhedronSolver[N],
  val faceBuilder:        FaceBuilder,
  val points:             IndexedSeq[NumVec[N]],
  val commonLimitsOption: Option[IndexedSeq[IntVec]],
  val basisOption:        Option[IndexedSeq[IntVec]],
  val illustrate:         Boolean,
  output:                 PrintWriter
)(implicit mp: MathProcessor[N])
    extends SolverPrinter[PolyhedronSolver[N]](solver, output) {

  override def solveFor(
      solver: PolyhedronSolver[N],
      output: PrintWriter
  ) = {
    output.println(title("Polyhedron computation"))

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

    val lookupTable = solver.solve(points, commonLimitsOption, basisOption)
    printLookupTable(lookupTable, output)

    val facesMap = faceBuilder.faces(lookupTable, dimension)

    var upperDimFaces = IndexedSeq.empty[Face]
    for ((faceDim, currDimFaces) <- facesMap) {
      val currDimFacesList = currDimFaces.toIndexedSeq.sorted
      output.println(subheader("Face Dimension: " + faceDim))
      currDimFacesList.foreachWithIndex { (face, idx) =>
        output.println(MessageFormat.format("  {0})\t{1}", int2Integer(idx), face.makeString(upperDimFaces)))
      }
      upperDimFaces = currDimFacesList
    }

    if (dimension <= 3 && illustrate) {
      illustrate(dimension, points, facesMap, 85)
    }
  }

  private def printLookupTable(
      lookupTable: KeyTable[IntVec, Int, Boolean],
      output:      PrintWriter
  ) = {
    output.println(header("Compliance table:"))
    val strTable = lookupTable.rowKeys.zipWithIndex.foldLeft(KeyTable.empty[String, String, String]) {
      case (table, (rowKey, rowIdx)) =>
        val rowKeyStr = MessageFormat.format("N{0} = {1}", int2Integer(rowIdx), rowKey.toTupleString)
        lookupTable.colKeys.zipWithIndex.foldLeft(table) {
          case (table, (colKey, colIdx)) =>
            val colKeyStr = MessageFormat.format(" Q{0}", int2Integer(colIdx))
            val isContained = lookupTable.get(rowKey, colKey).getOrElse(false)
            val toAdd = if (isContained) " +" else " -"
            table + (rowKeyStr, colKeyStr, toAdd)
        }
    }
    // Skip "+-----+----+" lines
    val lines = strTable.toString.split("\n")
    for (line <- lines if !(line startsWith "+-")) {
      output.println(line)
    }
  }

  private def illustrate(
      dim:      Int,
      points:   Seq[NumVec[N]],
      facesMap: Map[Int, Set[Face]],
      freq:     Int
  ) = {
    val points3d = points map (p => PointUtils.p3d(p))
    val lines = collectLineCorners(facesMap(1), points)
    val borderEdgesAlt = (lines map (_.pointIndices map points3d)).flatten
    val illustrFrames = Seq(
      doDrawFrame("All-Vs-All", points3d, PolyRenderer.AllVsAll, 0, 150, 512, 512, dim == 2),
      doDrawFrame("Convex Hull", borderEdgesAlt, PolyRenderer.Triangles, 512, 150, 512, 512, dim == 2)
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

  private def doDrawFrame(
      title:     String,
      pts:       Seq[Point3d],
      mode:      Int,
      positionX: Int,
      positionY: Int,
      width:     Int,
      height:    Int,
      is2d:      Boolean
  ): Frame = {
    val polyRenderer = new PolyRenderer(pts, mode, is2d)
    val frame = new MainFrame(polyRenderer, width, height)
    frame.setTitle(title)
    frame.setLocation(positionX, positionY)
    frame
  }

  private def collectLineCorners(faces: Iterable[Face], points: Seq[NumVec[N]]): Seq[Face] = {
    faces.map(s => if (s.size == 2) s else getLineCorners(s, points)).toSeq
  }

  private def getLineCorners(face: Face, points: Seq[NumVec[N]]): Face = {
    def getLesserAndGreaterPoints(
        facePtsIndices: List[Int],
        currDim:        Int, lesserPtIdx: Int, greaterPtIdx: Int
    ): (Int, Int) =
      facePtsIndices match {
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
    val facePtsIndices = face.pointIndices.toIndexedSeq
    val dimension = points(0).size
    for (t <- 0 until dimension) {
      val (lesserPtIdx, greaterPtIdx) = getLesserAndGreaterPoints(facePtsIndices.toList, t, facePtsIndices(0), facePtsIndices(0))
      if (lesserPtIdx != greaterPtIdx)
        return new Face(Seq(lesserPtIdx, greaterPtIdx)).addUpperFaces(face.upperFaces)
    }
    // If this is the case - all points are same
    return new Face(Seq(facePtsIndices.head)).addUpperFaces(face.upperFaces)
  }
}
