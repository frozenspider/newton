package org.newtonpolyhedron

import org.newtonpolyhedron.entity.ExecutorRunnable
import java.io.PrintWriter
import org.newtonpolyhedron.ex.UnknownModeException
import java.io.File
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.solve.surface.SurfaceBuilderImpl
import org.newtonpolyhedron.solve.cone.ConeSolverImpl
import org.newtonpolyhedron.solverprinters.PolyhedronSolverPrinter
import org.newtonpolyhedron.solve.poly.PolyMotzkinBurgerSolver
import org.newtonpolyhedron.entity.vector.MathVector
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.VectorFormat
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.entity.BigFrac
import java.io.FileReader
import java.util.Scanner
import java.util.regex.Pattern
import scala.util.control.BreakControl
import scala.io.Source

class NewtonLogicNew {
  /**
   * Starts the processing thread.
   *
   * @param path
   *            path to input file
   * @param mode
   *            working mode (see {@link WorkingMode})
   * @param illustrate
   *            try to illustrate the solution if possible
   * @param writer
   *            where to output data during work process
   * @return working thread (not started)
   * @throws WrongFormatException
   *             if file was malformed
   * @throws UnknownModeException
   *             if chosen mode has not yet been supported
   * @throws Exception
   *             if... whatever.
   */
  def start(path: String,
            mode: WorkingMode,
            illustrate: Boolean,
            writer: PrintWriter): Thread = {
    val file = new File(path)
    val solver = mode match {
      case WorkingMode.POLY_MOTZKIN_BURGER => launchPolyMotzkinBurger(file, illustrate, writer)
      //      case WorkingMode.POLY_INTERSECTION         => launchIntersection(file, writer)
      //      case WorkingMode.CONE                      => launchCone(file, writer)
      //      case WorkingMode.MATRIX_DET                => launchMatrixDet(file, writer)
      //      case WorkingMode.MATRIX_INVERSE            => launchMatrixInverse(file, writer)
      //      case WorkingMode.MATRIX_UNIMODULAR_ALPHA   => launchMatrixUniAlpha(file, writer)
      //      case WorkingMode.MATRIX_LAST_ROW_MINOR_GCD => launchMatrixMinorGCD(file, writer)
      case _                               => throw new UnknownModeException(mode)
    }
    return new Thread(new ExecutorRunnable(solver, writer), "MainSolver")
  }

  def launchPolyMotzkinBurger(file: File,
                              illustrate: Boolean,
                              writer: PrintWriter): SolverPrinter[_] = {
    val (pointList, commonLimits, basis) = readFromFile(file, FracMathVec.FracMathVecFormat)
    val coneSolver = new ConeSolverImpl
    val polySolver = new PolyMotzkinBurgerSolver(coneSolver)
    val surfaceBuilder = new SurfaceBuilderImpl
    new PolyhedronSolverPrinter(polySolver, surfaceBuilder, pointList, commonLimits, basis, illustrate, writer)
  }

  /**@return pointList, commonLimits, basis*/
  private def readFromFile[C <: Ordered[C], V <: MathVector[C, V]](
    file: File, format: VectorFormat[C, V]): (IndexedSeq[V], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) = {
    val lines = {
      val src = Source.fromFile(file)
      val lines = src.getLines
      src.close
      lines.toStream map (_.replaceAll("[ \\t]", " ").trim) filter (!_.isEmpty)
    }
    val dim = Integer.parseInt(lines.head)
    val xInt = Array.ofDim[Long](dim)
    val x = format.createArrayOfZeros(dim)
    val CommentPat = "(@.*)".r
    val DollarPat = "\\$(.*)".r
    val SharpPat = "#(.*)".r
    def read(src: Seq[String],
             pointList: IndexedSeq[V],
             commonLimits: IndexedSeq[IntMathVec],
             basis: IndexedSeq[IntMathVec]): (IndexedSeq[V], IndexedSeq[IntMathVec], IndexedSeq[IntMathVec]) =
      src.headOption match {
        //      var j = 0
        //      while (scannerF.hasNext) {
        //        val st = scannerF.next()
        case None                => (pointList, commonLimits, basis)
        case Some(CommentPat(_)) => (pointList, commonLimits, basis)
        case Some(DollarPat(s))  => ???
        //          while (scannerF.hasNext()) {
        //            val stAlt = scannerF.next()
        //            if (stAlt.charAt(0) == '$') {
        //              break
        //            }
        //            xInt(j) = Long.parseLong(stAlt)
        //            j += 1
        //            if (j == dim) {
        //              j = 0
        //              commonLimits.add(new IntVector(xInt))
        //            }
        //          }
        //          j = 0
        //          continue
        case Some(SharpPat(s))   => ???
        //          while (scannerF.hasNext()) {
        //            val stAlt = scannerF.next()
        //            if (stAlt.length() == 0) {
        //              continue
        //            }
        //            if (stAlt.charAt(0) == '#') {
        //              break
        //            }
        //            xInt(j) = Long.parseLong(stAlt)
        //            j += 1
        //            if (j == dim) {
        //              j = 0
        //              basis.add(new IntVector(xInt))
        //            }
        //          }
        //          j = 0
        //          continue
        case Some(s) => {
          val parsed = s split " " map format.parseElement
          require(parsed.length == dim, "Incorrect line size: '" + s + "', while dim = " + dim)
          val vec = format makeVector parsed
          read(src.tail, pointList :+ vec, commonLimits, basis)
        }
      }
    read(lines.tail, IndexedSeq.empty, IndexedSeq.empty, IndexedSeq.empty)
  }
}