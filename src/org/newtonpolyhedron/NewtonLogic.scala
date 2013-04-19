package org.newtonpolyhedron

import java.io.File
import java.io.FileNotFoundException
import java.io.PrintWriter

import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.ExecutorRunnable
import org.newtonpolyhedron.entity.MatrixSupport
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.entity.vector.FracMathVec
import org.newtonpolyhedron.entity.vector.IntMathVec
import org.newtonpolyhedron.ex.UnknownModeException
import org.newtonpolyhedron.ex.WrongFormatException
import org.newtonpolyhedron.solve.cone._
import org.newtonpolyhedron.solve.matrixminorgcd.MatrixMinorGCDSolverImpl
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMakerImpl
import org.newtonpolyhedron.solve.poly._
import org.newtonpolyhedron.solve.polyinter._
import org.newtonpolyhedron.solve.surface._
import org.newtonpolyhedron.solverprinters._

class NewtonLogic {

  val intFmt = IntMathVec.IntMathVecFormat
  val fracFmt = FracMathVec.FracMathVecFormat
  val coneSolver: ConeSolver = new ConeSolverImpl

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
  @throws(classOf[FileNotFoundException])
  @throws(classOf[WrongFormatException])
  @throws(classOf[UnknownModeException])
  def makeThread(path: String,
                 mode: WorkingMode,
                 illustrate: Boolean,
                 writer: PrintWriter): Thread = {
    val file = new File(path)
    val solver = mode match {
      case WorkingMode.POLY_MOTZKIN_BURGER       => launchPolyMotzkinBurger(file, illustrate, writer)
      case WorkingMode.POLY_INTERSECTION         => launchIntersection(file, writer)
      case WorkingMode.CONE                      => launchCone(file, writer)
      case WorkingMode.MATRIX_DET                => launchMatrixDet(file, writer)
      case WorkingMode.MATRIX_INVERSE            => launchMatrixInverse(file, writer)
      case WorkingMode.MATRIX_UNIMODULAR_ALPHA   => launchMatrixUniAlpha(file, writer)
      case WorkingMode.MATRIX_LAST_ROW_MINOR_GCD => launchMatrixMinorGCD(file, writer)
      case _                                     => throw new UnknownModeException(mode)
    }
    return new Thread(new ExecutorRunnable(solver, writer), "MainSolver")
  }

  def launchPolyMotzkinBurger(file: File,
                              illustrate: Boolean,
                              writer: PrintWriter): SolverPrinter[_] = {
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromFile(file, fracFmt)
    val polySolver = new PolyMotzkinBurgerSolver(coneSolver)
    val surfaceBuilder = new SurfaceBuilderImpl
    new PolyhedronSolverPrinter(polySolver, surfaceBuilder, pointList, commonLimits, basis, illustrate, writer)
  }

  def launchIntersection(file: File,
                         writer: PrintWriter): SolverPrinter[_] = {
    val (polys, dim) = InputParser.parsePolysFromFile(file, fracFmt)
    val polySolver = new PolyIntersectionSolverImpl(coneSolver)
    new PolyIntersectionSolverPrinter(polySolver, polys, dim, writer)
  }

  def launchCone(file: File,
                 writer: PrintWriter): SolverPrinter[_] = {
    val (pointList, commonLimits, basis) = InputParser.parsePolyFromFile(file, intFmt)
    new ConeSolverPrinter(coneSolver, pointList, basis, writer)
  }

  def launchMatrixDet(file: File,
                      writer: PrintWriter): SolverPrinter[_] = {
    val (matrix, skipRow, skipCol) = InputParser.parseMatrixWithSkipFromFile(file, fracFmt, MatrixSupport.fromFracs)
    new MatrixDetSolverPrinter(matrix, skipRow, skipCol, writer)
  }

  def launchMatrixInverse(file: File,
                          writer: PrintWriter): SolverPrinter[_] = {
    val matrix = InputParser.parseMatrixFromFile(file, fracFmt, MatrixSupport.fromFracs)
    new MatrixInverseSolverPrinter(matrix, writer)
  }

  def launchMatrixUniAlpha(file: File,
                           writer: PrintWriter): SolverPrinter[_] = {
    val matrix = {
      val m = InputParser.parseMatrixFromFile(file, fracFmt, MatrixSupport.fromFracs)
      // Add all-zero row if necessary
      if (m.isSquare) m
      else if (m.rowNum != m.colNum - 1) throw new WrongFormatException("Pre-alpha matrix should have either d or d-1 rows")
      else m addRow (Seq.fill(m.colNum)(BigFrac.ZERO))
    }
    val uniMatrixMaker = new UnimodularMatrixMakerImpl
    new UnimodularMatrixMakerPrinter(uniMatrixMaker, matrix, writer)
  }

  def launchMatrixMinorGCD(file: File,
                           writer: PrintWriter): SolverPrinter[_] = {
    val matrix = InputParser.parseMatrixFromFile(file, fracFmt, MatrixSupport.fromFracs)
    val gcdMatrixSolver = new MatrixMinorGCDSolverImpl
    new MatrixMinorGCDSolverPrinter(gcdMatrixSolver, matrix, writer)
  }
}