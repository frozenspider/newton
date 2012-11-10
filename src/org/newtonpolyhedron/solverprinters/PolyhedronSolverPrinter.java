package org.newtonpolyhedron.solverprinters;

/*-
 * 1. Create a Canvas3D object
 * 2. Create a VirtualUniverse object
 * 3. Create a Locale object, attaching it to the VirtualUniverse object
 * 4. Construct a view branch graph
 * 		a. Create a View object
 * 		b. Create a ViewPlatform object
 * 		c. Create a PhysicalBody object
 * 		d. Create a PhysicalEnvironment object
 * 		e. Attach ViewPlatform, PhysicalBody, PhysicalEnvironment, and Canvas3D objects to View object
 * 5. Construct content branch graph(s)
 * 6. Compile branch graph(s)
 * 7. Insert subgraphs into the Locale
 */
import static java.text.MessageFormat.*;

import java.awt.Frame;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.vecmath.Point3d;

import org.fs.utils.collection.set.IndexedSet;
import org.fs.utils.collection.table.ArrayListKeyTable;
import org.fs.utils.collection.table.KeyTable;
import org.newtonpolyhedron.PointsLineApp;
import org.newtonpolyhedron.entity.SolverPrinter;
import org.newtonpolyhedron.entity.Surface;
import org.newtonpolyhedron.entity.vector.FractionVector;
import org.newtonpolyhedron.entity.vector.IntVector;
import org.newtonpolyhedron.solve.poly.PolyhedronSolver;
import org.newtonpolyhedron.solve.surface.SurfaceBuilder;
import org.newtonpolyhedron.utils.PointUtils;

public class PolyhedronSolverPrinter extends SolverPrinter <PolyhedronSolver> {
	
	private final Frame[]				illustrFrames	= {null, null, null};
	private final int					frequency		= 85;
	
	private final SurfaceBuilder		surfaceBuilder;
	private final List <FractionVector>	points;
	private final List <IntVector>		commonLimits;
	private final List <IntVector>		basis;
	private final boolean				illustrate;
	
	
	public PolyhedronSolverPrinter(
			final PolyhedronSolver solver,
			final SurfaceBuilder surfaceBuilder,
			final List <FractionVector> points,
			final List <IntVector> commonLimits,
			final List <IntVector> basis,
			final boolean illustrate,
			final PrintWriter output) {
		super(solver, output);
		this.surfaceBuilder = surfaceBuilder;
		this.points = points;
		this.commonLimits = commonLimits;
		this.basis = basis;
		this.illustrate = illustrate;
	}
	
	@Override
	protected void solveFor(final PolyhedronSolver solver, final PrintWriter output)
			throws Exception {
		output.println("\n\n\n --- === Begin === ---\n");
		output.println("=== Original points: ===");
		final int dim = points.get(0).getDim();
		for (int i = 0; i < points.size(); i++) {
			output.println(format(" Q{0} = {1}", i, points.get(i)));
		}
		output.println("=== Common limits: ===");
		if (commonLimits != null && commonLimits.size() > 0) {
			for (int i = 0; i < commonLimits.size(); i++) {
				output.println(format(" L{0} = {1}", i, commonLimits.get(i)));
			}
		} else {
			output.println(" (none)");
		}
		
		final KeyTable <IntVector, Integer, Boolean> lookupTable = solver.solve(points,
				commonLimits, basis, output);
		printLookupTable(lookupTable, output);
		
		final Map <Integer, IndexedSet <Surface>> surfacesMap = surfaceBuilder.getSurfaces(
				lookupTable, dim);
		
		for (final Entry <Integer, IndexedSet <Surface>> entry : surfacesMap.entrySet()) {
			output.println("\nSurface Dimension: " + entry.getKey());
			int idx = 0;
			for (final Surface surface : entry.getValue()) {
				output.println(format("  {0})\t{1}", idx++, surface.toString()));
			}
		}
		
		if (dim <= 3 && illustrate) {
			final IndexedSet <Surface> oneDimSurfaces = surfacesMap.get(1);
			
			final List <Point3d> points3d = new ArrayList <Point3d>(points.size());
			for (final FractionVector p : points) {
				points3d.add(PointUtils.toPoint3d(p));
			}
			illustrFrames[0] = PointsLineApp.doDrawFrame(points3d, null, PointsLineApp.ALL_VS_ALL,
					0, 150, 512, 512, dim == 2);
			final List <Point3d> borderEdgesAlt = new ArrayList <Point3d>();
			for (final Surface border : oneDimSurfaces) {
				for (final int borderPtIdx : border.getPointIdxList()) {
					borderEdgesAlt.add(PointUtils.toPoint3d(points.get(borderPtIdx)));
				}
				
			}
			illustrFrames[2] = PointsLineApp.doDrawFrame(borderEdgesAlt, null,
					PointsLineApp.TRIANGLES, 512, 150, 512, 512, dim == 2);
			
			try {
				while (!Thread.interrupted()) {
					// final Graphics g=strategy.getDrawGraphics();
					// strategy.show();
					// textarea.setText(String.format("%d",i));
					Thread.sleep(frequency);
				}
			} catch(final InterruptedException ex) {
				// NOOP
			} finally {
				for (final Frame frame : illustrFrames) {
					if (frame != null) {
						frame.dispose();
					}
				}
			}
		}
	}
	
	public static void printLookupTable(
			final KeyTable <IntVector, Integer, Boolean> lookupTable,
			final PrintWriter output) {
		final KeyTable <String, String, String> strTable = new ArrayListKeyTable <String, String, String>();
		int rowIdx = 0;
		for (final IntVector rowKey : lookupTable.rowKeyList()) {
			final String rowKeyStr = format("N{0} = {1}", rowIdx, rowKey);
			int colIdx = 0;
			for (final Integer colKey : lookupTable.colKeyList()) {
				final String colKeyStr = format(" Q{0}", colIdx);
				final Boolean value = lookupTable.get(rowKey, colKey);
				if (value == Boolean.TRUE) {
					strTable.put(rowKeyStr, colKeyStr, " +");
				} else {
					strTable.put(rowKeyStr, colKeyStr, " -");
				}
				++colIdx;
			}
			++rowIdx;
		}
		final String str = strTable.toString();
		// Skip "+-----+----+" lines
		final String[] lines = str.split("\n");
		for (final String line : lines) {
			if (!line.startsWith("+-")) {
				output.println(line);
			}
		}
	}
}
