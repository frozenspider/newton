package org.newtonpolyhedron

import java.io.File
import java.io.FileNotFoundException
import java.io.PrintWriter

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.entity.SolverPrinter
import org.newtonpolyhedron.ex.WrongFormatException
import org.newtonpolyhedron.solve.changevars.ChangerOfVariablesImpl
import org.newtonpolyhedron.solve.cone._
import org.newtonpolyhedron.solve.eqsys.EqSystemChainSolver
import org.newtonpolyhedron.solve.eqsys.ManualEqSystemSolver
import org.newtonpolyhedron.solve.eqsys.SimpleEqSystemSolverImpl
import org.newtonpolyhedron.solve.matrixminorgcd.MatrixMinorGCDSolverImpl
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMakerImpl
import org.newtonpolyhedron.solve.poly._
import org.newtonpolyhedron.solve.polyinter._
import org.newtonpolyhedron.solve.power.PowerTransformationSolverImpl
import org.newtonpolyhedron.solve.surface._
import org.newtonpolyhedron.solverprinters._
import org.newtonpolyhedron.ui.eqsys.EqSystemSolutionDialogInput
import org.newtonpolyhedron.utils.parsing.ParseFormats._

/**
 * Serves for launching worker threads for specific working modes, used from GUI.
 *
 * @author FS
 */
class WorkerLauncher[N <: MPNumber](implicit mp: MathProcessor[N]) {

  lazy val systemOfEqSolverChain =
    new EqSystemChainSolver(Seq(
      new SimpleEqSystemSolverImpl,
      new ManualEqSystemSolver(new EqSystemSolutionDialogInput)
    ))

  lazy val coneSolver: ConeSolver = new MotzkinBurger

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
   * @throws Exception
   *             if... whatever.
   */
  @throws(classOf[FileNotFoundException])
  @throws(classOf[WrongFormatException])
  def makeThread(
      path:       String,
      mode:       WorkingMode.Value,
      illustrate: Boolean,
      writer:     PrintWriter
  ): Thread = {
    val file = new File(path)
    val solver = mode match {
      case WorkingMode.Poly                   => launchPolyMotzkinBurger(file, illustrate, writer)
      case WorkingMode.PolyIntersection       => launchIntersection(file, writer)
      case WorkingMode.Cone                   => launchCone(file, writer)
      case WorkingMode.MatrixDet              => launchMatrixDet(file, writer)
      case WorkingMode.MatrixInv              => launchMatrixInverse(file, writer)
      case WorkingMode.MatrixUnimodularAlpha  => launchMatrixUniAlpha(file, writer)
      case WorkingMode.MatrixLastRowMinorsGCD => launchMatrixMinorGCD(file, writer)
      case WorkingMode.PowerTransformation    => launchPowerTransformation(file, writer)
    }
    return new Thread(new ExecutorRunnable(solver, writer), "MainSolver")
  }

  private def launchPolyMotzkinBurger(
      file:       File,
      illustrate: Boolean,
      writer:     PrintWriter
  ): SolverPrinter[_] = {
    val (pointList, commonLimitsOption, basisOption) = InputParser.parsePolyFromFile(file)(parseNum)
    val polySolver = new PolyMotzkinBurgerSolver(coneSolver)
    val surfaceBuilder = new SurfaceBuilderImpl
    new PolyhedronSolverPrinter(polySolver, surfaceBuilder, pointList, commonLimitsOption, basisOption, illustrate, writer)
  }

  private def launchIntersection(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val (polys, dim) = InputParser.parsePolysFromFile(file)(parseNum)
    val polySolver = new PolyIntersectionSolverImpl(coneSolver)
    new PolyIntersectionSolverPrinter(polySolver, polys, dim, writer)
  }

  private def launchCone(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val (pointList, _, basisOption) = InputParser.parsePolyFromFile(file)(parseInt)
    new ConeSolverPrinter(coneSolver, pointList, basisOption, writer)
  }

  private def launchMatrixDet(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val (matrix, skipRow, skipCol) = InputParser.parseMatrixWithSkipFromFile(file, Matrix.apply[N], parseNum[N])
    new MatrixDetSolverPrinter(matrix, skipRow, skipCol, writer)
  }

  private def launchMatrixInverse(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val matrix = InputParser.parseMatrixFromFile(file, Matrix.apply[N], parseNum[N])
    new MatrixInverseSolverPrinter(matrix, writer)
  }

  private def launchMatrixUniAlpha(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val matrix = {
      val m = InputParser.parseMatrixFromFile(file, Matrix.apply[N], parseNum[N])
      // Add all-zero row if necessary
      if (m.isSquare) m
      else if (m.rowCount != m.colCount - 1) throw new WrongFormatException("Pre-alpha matrix should have either d or d-1 rows")
      else m addRow (Seq.fill(m.colCount)(mp.zero))
    }
    val uniMatrixMaker = new UnimodularMatrixMakerImpl[N]
    new UnimodularMatrixMakerPrinter[N](uniMatrixMaker, matrix, writer)
  }

  private def launchMatrixMinorGCD(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val matrix = InputParser.parseMatrixFromFile(file, Matrix.apply[N], parseNum[N])
    val gcdMatrixSolver = new MatrixMinorGCDSolverImpl
    new MatrixMinorGCDSolverPrinter(gcdMatrixSolver, matrix, writer)
  }

  private def launchPowerTransformation(
      file:   File,
      writer: PrintWriter
  ): SolverPrinter[_] = {
    val (polys, pts) = InputParser.parsePowerTransfBaseFromFile(file)
    val powTransfSolver = new PowerTransformationSolverImpl(
      new UnimodularMatrixMakerImpl,
      systemOfEqSolverChain
    )
    new PowerTransformationSolverPrinter(
      powTransfSolver, new ChangerOfVariablesImpl, polys, pts, writer
    )
  }

  private class ExecutorRunnable(solver: SolverPrinter[_], output: PrintWriter) extends Runnable {

    override def run: Unit =
      try
        solver.solveAndPrint
      catch {
        case th: Throwable => th.printStackTrace(output)
      }
  }
}
