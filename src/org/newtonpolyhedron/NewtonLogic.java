package org.newtonpolyhedron;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Pattern;

import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.fraction.BigFractionFormat;
import org.apache.commons.math3.linear.FieldMatrix;
import org.newtonpolyhedron.entity.ExecutorRunnable;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.entity.vector.AbstractVector;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.FractionVectorFormat;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.entity.vector.IntVectorFormat;
import org.newtonpolyhedron.entity.vector.VectorFormat;
import org.newtonpolyhedron.ex.UnknownModeException;
import org.newtonpolyhedron.ex.WrongFormatException;
import org.newtonpolyhedron.solve.cone.ConeSolver;
import org.newtonpolyhedron.solve.cone.ConeSolverImpl;
import org.newtonpolyhedron.solve.matrixminorgcd.MatrizMinorGCDSolver;
import org.newtonpolyhedron.solve.matrixminorgcd.MatrizMinorGCDSolverImpl;
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMaker;
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMakerImpl;
import org.newtonpolyhedron.solve.poly.PolyMotzkinBurgerSolver;
import org.newtonpolyhedron.solve.poly.PolyhedronSolver;
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolver;
import org.newtonpolyhedron.solve.polyinter.PolyIntersectionSolverImpl;
import org.newtonpolyhedron.solve.surface.SurfaceBuilder;
import org.newtonpolyhedron.solve.surface.SurfaceBuilderImpl;
import org.newtonpolyhedron.solverprinters.ConeSolverPrinter;
import org.newtonpolyhedron.solverprinters.MatrixDetSolverPrinter;
import org.newtonpolyhedron.solverprinters.MatrixInverseSolverPrinter;
import org.newtonpolyhedron.solverprinters.MatrizMinorGCDSolverPrinter;
import org.newtonpolyhedron.solverprinters.PolyIntersectionSolverPrinter;
import org.newtonpolyhedron.solverprinters.PolyhedronSolverPrinter;
import org.newtonpolyhedron.solverprinters.UnimodularMatrixMakerPrinter;
import org.newtonpolyhedron.utils.MatrixUtils;

public class NewtonLogic {
	
	private Thread	workingThread	= null;
	
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
	 * @throws WrongFormatException
	 *             if file was malformed
	 * @throws UnknownModeException
	 *             if chosen mode has not yet been supported
	 * @throws Exception
	 *             if... whatever.
	 */
	public void start(
			final String path,
			final WorkingMode mode,
			final boolean illustrate,
			final PrintWriter writer) throws WrongFormatException, UnknownModeException, Exception {
		final File file = new File(path);
		final SolverPrinter <?> solver;
		switch (mode) {
			case POLY_MOTZKIN_BURGER:
				solver = launchPolyMotzkinBurger(file, illustrate, writer);
				break;
			case POLY_INTERSECTION:
				solver = launchIntersection(file, writer);
				break;
			case CONE:
				solver = launchCone(file, writer);
				break;
			case MATRIX_DET:
				solver = launchMatrixDet(file, writer);
				break;
			case MATRIX_INVERSE:
				solver = launchMatrixInverse(file, writer);
				break;
			case MATRIX_UNIMODULAR_ALPHA:
				solver = launchMatrixUniAlpha(file, writer);
				break;
			case MATRIX_LAST_ROW_MINOR_GCD:
				solver = launchMatrixMinorGCD(file, writer);
				break;
			default:
				throw new UnknownModeException(mode);
		}
		workingThread = new Thread(new ExecutorRunnable(solver, writer), "MainSolver");
		workingThread.start();
	}
	
	public void stop() {
		workingThread.interrupt();
		// workingThread.join();
	}
	
	private SolverPrinter <?> launchPolyMotzkinBurger(
			final File file,
			final boolean illustrate,
			final PrintWriter writer) throws IOException {
		final List <FractionVector> pointList = new ArrayList <FractionVector>();
		final List <IntVector> commonLimits = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		readFromFile(file, pointList, commonLimits, basis, FractionVectorFormat.getInstance());
		final ConeSolver coneSolver = new ConeSolverImpl();
		final PolyhedronSolver solver = new PolyMotzkinBurgerSolver(coneSolver);
		final SurfaceBuilder surfaceBuilder = new SurfaceBuilderImpl();
		return new PolyhedronSolverPrinter(solver, surfaceBuilder, pointList, commonLimits, basis,
				illustrate, writer);
	}
	
	private SolverPrinter <?> launchIntersection(final File file, final PrintWriter writer)
			throws IOException {
		final BigFractionFormat frFormat = FractionVectorFormat.getFractionFormat();
		final List <List <FractionVector>> polyhedrons = new ArrayList <List <FractionVector>>();
		final Reader inputF = new FileReader(file);
		final int dim;
		try {
			final Scanner scannerF = new Scanner(inputF);
			final Pattern delim = Pattern.compile("[ *\\n\\r]");
			scannerF.useDelimiter(delim);
			String d = "";
			while (d.length() == 0) {
				d = scannerF.next();
			}
			dim = Integer.parseInt(d);
			final BigFraction[] x = new BigFraction[dim];
			Arrays.fill(x, BigFraction.ZERO);
			int j = 0;
			List <FractionVector> currPoly = new ArrayList <FractionVector>();
			while (scannerF.hasNext()) {
				final String st = scannerF.next();
				if (st.length() == 0) {
					continue;
				}
				if (st.charAt(0) == '@') {
					break;
				}
				if (st.charAt(0) == '%') {
					polyhedrons.add(currPoly);
					currPoly = new ArrayList <FractionVector>();
					continue;
				}
				
				x[j++] = frFormat.parse(st);
				if (j == dim) {
					j = 0;
					currPoly.add(new FractionVector(x));
				}
			}
			if (!currPoly.isEmpty()) {
				polyhedrons.add(currPoly);
			}
		} finally {
			inputF.close();
		}
		final ConeSolver coneSolver = new ConeSolverImpl();
		final PolyIntersectionSolver solver = new PolyIntersectionSolverImpl(coneSolver);
		return new PolyIntersectionSolverPrinter(solver, polyhedrons, dim, writer);
	}
	
	private SolverPrinter <?> launchCone(final File file, final PrintWriter writer)
			throws IOException {
		final List <IntVector> pointList = new ArrayList <IntVector>();
		final List <IntVector> basis = new ArrayList <IntVector>();
		readFromFile(file, pointList, new ArrayList <IntVector>(), basis,
				IntVectorFormat.getInstance());
		final ConeSolver solver = new ConeSolverImpl();
		return new ConeSolverPrinter(solver, pointList, basis, writer);
	}
	
	private SolverPrinter <?> launchMatrixDet(final File file, final PrintWriter writer)
			throws IOException {
		final BigFractionFormat frFormat = FractionVectorFormat.getFractionFormat();
		final List <BigFraction[]> listMatrix = new ArrayList <BigFraction[]>();
		final int skipRow;
		final int skipCol;
		final Reader inputF = new FileReader(file);
		try {
			final Scanner scannerF = new Scanner(inputF);
			final Pattern delim = Pattern.compile("[ \\n*\\r]");
			scannerF.useDelimiter(delim);
			final int dim = Integer.parseInt(scannerF.next());
			scannerF.next();
			skipRow = Integer.parseInt(scannerF.next());
			skipCol = Integer.parseInt(scannerF.next());
			scannerF.next();
			final BigFraction[] x = new BigFraction[dim];
			int j = 0;
			while (scannerF.hasNext()) {
				final String st = scannerF.next();
				if (st.length() == 0) {
					continue;
				}
				if (st.charAt(0) == '@') {
					break;
				}
				x[j++] = frFormat.parse(st);
				if (j == dim) {
					j = 0;
					listMatrix.add(Arrays.copyOf(x, x.length));
				}
			}
		} finally {
			inputF.close();
		}
		final FieldMatrix <BigFraction> arrayMatrix = MatrixUtils.create(listMatrix.toArray(new BigFraction[0][0]));
		return new MatrixDetSolverPrinter(arrayMatrix, skipRow, skipCol, writer);
	}
	
	private SolverPrinter <?> launchMatrixInverse(final File file, final PrintWriter writer)
			throws IOException {
		final BigFractionFormat frFormat = FractionVectorFormat.getFractionFormat();
		final List <BigFraction[]> listMatrix = new ArrayList <BigFraction[]>();
		final Reader inputF = new FileReader(file);
		try {
			final Scanner scannerF = new Scanner(inputF);
			final Pattern delim = Pattern.compile("[ \\n*\\r]");
			scannerF.useDelimiter(delim);
			final int dim = Integer.parseInt(scannerF.next());
			scannerF.next();
			final BigFraction[] x = new BigFraction[dim];
			int j = 0;
			while (scannerF.hasNext()) {
				final String st = scannerF.next();
				if (st.length() == 0) {
					continue;
				}
				if (st.charAt(0) == '@') {
					break;
				}
				x[j++] = frFormat.parse(st);
				if (j == dim) {
					j = 0;
					listMatrix.add(Arrays.copyOf(x, x.length));
				}
			}
		} finally {
			inputF.close();
		}
		final FieldMatrix <BigFraction> arrayMatrix = MatrixUtils.create(listMatrix.toArray(new BigFraction[0][0]));
		return new MatrixInverseSolverPrinter(arrayMatrix, writer);
	}
	
	private SolverPrinter <?> launchMatrixUniAlpha(final File file, final PrintWriter writer)
			throws IOException {
		final BigFractionFormat frFormat = FractionVectorFormat.getFractionFormat();
		final FieldMatrix <BigFraction> baseMatrix;
		final Reader inputF = new FileReader(file);
		try {
			final Scanner scannerF = new Scanner(inputF);
			final Pattern delim = Pattern.compile("[ \\n\\r]");
			scannerF.useDelimiter(delim);
			final int dim = Integer.parseInt(scannerF.next());
			baseMatrix = MatrixUtils.create(dim);
			
			// Note: Last row is zero-row
			
			int row = 0;
			int col = 0;
			while (scannerF.hasNext()) {
				final String st = scannerF.next();
				if (st.length() == 0) {
					continue;
				}
				if (st.charAt(0) == '@') {
					break;
				}
				if (row >= dim - 1) throw new IllegalArgumentException("Too many data in matrix");
				baseMatrix.setEntry(row, col, frFormat.parse(st));
				++col;
				if (col == dim) {
					col = 0;
					++row;
				}
			}
			if (col != 0) throw new IllegalArgumentException("Matrix wasn't full");
		} finally {
			inputF.close();
		}
		final UnimodularMatrixMaker matrixMaker = new UnimodularMatrixMakerImpl();
		return new UnimodularMatrixMakerPrinter(matrixMaker, baseMatrix, writer);
	}
	
	private SolverPrinter <?> launchMatrixMinorGCD(final File file, final PrintWriter writer)
			throws IOException {
		final BigFractionFormat frFormat = FractionVectorFormat.getFractionFormat();
		final FieldMatrix <BigFraction> baseMatrix;
		final Reader inputF = new FileReader(file);
		try {
			final Scanner scannerF = new Scanner(inputF);
			final Pattern delim = Pattern.compile("[ \\n\\r]");
			scannerF.useDelimiter(delim);
			final int dim = Integer.parseInt(scannerF.next());
			baseMatrix = MatrixUtils.create(dim);
			
			// Note: Last row is zero-row
			
			int row = 0;
			int col = 0;
			while (scannerF.hasNext()) {
				final String st = scannerF.next();
				if (st.length() == 0) {
					continue;
				}
				if (st.charAt(0) == '@') {
					break;
				}
				if (row >= dim - 1) throw new IllegalArgumentException("Too many data in matrix");
				baseMatrix.setEntry(row, col, frFormat.parse(st));
				++col;
				if (col == dim) {
					col = 0;
					++row;
				}
			}
			if (col != 0) throw new IllegalArgumentException("Matrix wasn't full");
		} finally {
			inputF.close();
		}
		final MatrizMinorGCDSolver matrixSolver = new MatrizMinorGCDSolverImpl();
		return new MatrizMinorGCDSolverPrinter(matrixSolver, baseMatrix, writer);
	}
	
	private <Comp extends Comparable <Comp>,Vec extends AbstractVector <Comp, Vec>>void readFromFile(
			final File file,
			final List <Vec> pointList,
			final List <IntVector> commonLimits,
			final List <IntVector> basis,
			final VectorFormat <Comp, Vec> format) throws IOException {
		final Reader inputF = new FileReader(file);
		try {
			final Scanner scannerF = new Scanner(inputF);
			scannerF.useDelimiter(Pattern.compile("[ \\t\\n\\r]"));
			String d = "";
			while (d.length() == 0) {
				d = scannerF.next();
			}
			final int dim = Integer.parseInt(d);
			final long[] xInt = new long[dim];
			final Comp[] x = format.createArrayOfZeros(dim);
			int j = 0;
			while (scannerF.hasNext()) {
				final String st = scannerF.next();
				if (st.length() == 0) {
					continue;
				}
				if (st.charAt(0) == '@') {
					break;
				}
				if (st.charAt(0) == '$') {
					while (scannerF.hasNext()) {
						final String stAlt = scannerF.next();
						if (stAlt.length() == 0) {
							continue;
						}
						if (stAlt.charAt(0) == '$') {
							break;
						}
						xInt[j++] = Long.parseLong(stAlt);
						if (j == dim) {
							j = 0;
							commonLimits.add(new IntVector(xInt));
						}
					}
					j = 0;
					continue;
				}
				if (st.charAt(0) == '#') {
					while (scannerF.hasNext()) {
						final String stAlt = scannerF.next();
						if (stAlt.length() == 0) {
							continue;
						}
						if (stAlt.charAt(0) == '#') {
							break;
						}
						xInt[j++] = Long.parseLong(stAlt);
						if (j == dim) {
							j = 0;
							basis.add(new IntVector(xInt));
						}
					}
					j = 0;
					continue;
				}
				x[j++] = format.parseElement(st);
				if (j == dim) {
					j = 0;
					pointList.add(format.makeVector(x));
				}
			}
		} finally {
			inputF.close();
		}
	}
}
