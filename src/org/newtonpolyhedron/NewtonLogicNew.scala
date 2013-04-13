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
import scala.util.matching.Regex
import org.newtonpolyhedron.entity.vector.IntMathVec.IntMathVecFormat
import org.newtonpolyhedron.solverprinters.ConeSolverPrinter
import org.newtonpolyhedron.solverprinters.ConeSolverPrinter

class NewtonLogicNew {

  val intFmt = IntMathVec.IntMathVecFormat
  val fracFmt = FracMathVec.FracMathVecFormat
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
    val (pointList, commonLimits, basis) = InputParser.readPolyFromFile(file, fracFmt)
    val coneSolver = new ConeSolverImpl
    val polySolver = new PolyMotzkinBurgerSolver(coneSolver)
    val surfaceBuilder = new SurfaceBuilderImpl
    new PolyhedronSolverPrinter(polySolver, surfaceBuilder, pointList, commonLimits, basis, illustrate, writer)
  }

  def launchIntersection(file: File,
                         writer: PrintWriter): SolverPrinter[_] = {
    ???
  }

  def launchCone(file: File,
                 writer: PrintWriter): SolverPrinter[_] = {
    val (pointList, commonLimits, basis) = InputParser.readPolyFromFile(file, intFmt)
    val coneSolver = new ConeSolverImpl
    new ConeSolverPrinter(coneSolver, pointList, basis, writer)
  }

  def launchMatrixDet(file: File,
                      writer: PrintWriter): SolverPrinter[_] = {
    ???
  }

  def launchMatrixInverse(file: File,
                          writer: PrintWriter): SolverPrinter[_] = {
    ???
  }

  def launchMatrixUniAlpha(file: File,
                           writer: PrintWriter): SolverPrinter[_] = {
    ???
  }

  def launchMatrixMinorGCD(file: File,
                           writer: PrintWriter): SolverPrinter[_] = {
    ???
  }
}