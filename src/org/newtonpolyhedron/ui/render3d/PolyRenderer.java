package org.newtonpolyhedron.ui.render3d;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.media.j3d.*;
import javax.swing.JApplet;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Point3f;

import com.sun.j3d.utils.applet.MainFrame;
import com.sun.j3d.utils.universe.SimpleUniverse;

@SuppressWarnings("serial")
public class PolyRenderer extends JApplet {
	
	public static final int	ALL_VS_ALL	= 0;
	public static final int	TRIANGLES	= 1;
	
	/**
	 * Create a simple scene and attach it to the virtual universe
	 * 
	 * @param pts
	 *            points to draw
	 * @param mode
	 *            drawing mode
	 * @param is2d
	 *            whether or not this image should be 2d
	 */
	public PolyRenderer(final List <Point3d> pts, final int mode, final boolean is2d) {
		final GraphicsConfiguration config = SimpleUniverse.getPreferredConfiguration();
		final Canvas3D canvas3D = new Canvas3D(config);
		setLayout(new BorderLayout());
		add("Center", canvas3D);
		final BranchGroup scene = createSceneGraph(pts, mode, is2d);
		// SimpleUniverse is a Convenience Utility class
		final SimpleUniverse universe = new SimpleUniverse(canvas3D);
		// This will move the ViewPlatform back a bit so the objects in the scene can be viewed.
		universe.getViewingPlatform().setNominalViewingTransform();
		universe.addBranchGraph(scene);
	}
	
	/** Create scene graph branch group */
	private BranchGroup createSceneGraph(
			final List <Point3d> pts,
			final int mode,
			final boolean is2d) {
		final BranchGroup objRoot = new BranchGroup();
		// Create the transform group node and initialize it to the identity.
		// Enable the TRANSFORM_WRITE capability so that our behavior code can modify it at runtime.
		// Add it to the root of the subgraph.
		final Transform3D rotate = new Transform3D();
		final Transform3D tempRotate = new Transform3D();
		if (!is2d) {
			rotate.rotX(Math.PI * 1.75d);
			tempRotate.rotZ(Math.PI * 1.15d);
			rotate.mul(tempRotate);
		}
		final TransformGroup objRotate = new TransformGroup(rotate);
		final TransformGroup objSpin = new TransformGroup();
		objSpin.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
		final Transform3D scale = new Transform3D();
		scale.setScale(0.1d);
		final TransformGroup objScale = new TransformGroup(scale);
		objRoot.addChild(objScale);
		objScale.addChild(objRotate);
		objRotate.addChild(objSpin);
		objSpin.addChild(new PointDrawer(pts, mode, is2d));
		// Text2D testText = new Text2D("Testing text", new Color3f(1.0f, 1.0f, 0.0f),
		// "Courier New", 150, 0);
		// objSpin.addChild(testText);
		/*
		 * Create a new Behavior object that will perform the desired operation on the specified
		 * transform object and add it into the scene graph.
		 */
		final Transform3D zAxis = new Transform3D();
		zAxis.rotX(Math.PI / 2.0d);
		final Alpha rotationAlpha = new Alpha(-1, 20000);
		final BoundingSphere bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);
		
		final Background bgWhite = new Background(new Color3f(0.9f, 0.9f, 0.9f));
		bgWhite.setApplicationBounds(bounds);
		objRoot.addChild(bgWhite);
		if (!is2d) {
			final RotationInterpolator rotator = new RotationInterpolator(rotationAlpha, objSpin,
					zAxis, 0.0f, (float) Math.PI * 2.0f);
			rotator.setSchedulingBounds(bounds);
			objSpin.addChild(rotator);
		}
		// Let Java 3D perform optimizations on this scene graph.
		objRoot.compile();
		return objRoot;
	}
	
	public class PointDrawer extends Shape3D {
		
		public PointDrawer(final List <Point3d> pts, final int mode, final boolean is2d) {
			this.setGeometry(createAxisLines(is2d));
			switch (mode) {
				case ALL_VS_ALL:
					this.addGeometry(createLinesAllVsAll(pts, null));
					break;
				case TRIANGLES:
					this.addGeometry(createTriangles(pts, Color.BLUE));
					break;
				default:
					throw new IllegalArgumentException("Unknown mode: " + mode);
			}
			this.setAppearance(createPointsAppearance());
		}
		
		private Appearance createPointsAppearance() {
			final Appearance appearance = new Appearance();
			final PolygonAttributes polyAttrib = new PolygonAttributes();
			polyAttrib.setPolygonMode(PolygonAttributes.POLYGON_LINE);
			appearance.setPolygonAttributes(polyAttrib);
			return appearance;
		}
		
		private Geometry createAxisLines(final boolean is2d) {
			final float len = 6.5f;
			final float dif = 0.2f;
			final List <Point3f> axisX = new ArrayList <Point3f>();
			axisX.add(new Point3f(0.0f, 0.0f, 0.0f));
			axisX.add(new Point3f(len, 0.0f, 0.0f));
			axisX.add(new Point3f(len, 0.0f, 0.0f));
			axisX.add(new Point3f(len - dif, dif, 0.0f));
			axisX.add(new Point3f(len, 0.0f, 0.0f));
			axisX.add(new Point3f(len - dif, -dif, 0.0f));
			if (!is2d) {
				axisX.add(new Point3f(len, 0.0f, 0.0f));
				axisX.add(new Point3f(len - dif, 0.0f, dif));
				axisX.add(new Point3f(len, 0.0f, 0.0f));
				axisX.add(new Point3f(len - dif, 0.0f, -dif));
			}
			axisX.add(new Point3f(0.0f, 0.0f, 0.0f));
			axisX.add(new Point3f(0.0f, len, 0.0f));
			axisX.add(new Point3f(0.0f, len, 0.0f));
			axisX.add(new Point3f(dif, len - dif, 0.0f));
			axisX.add(new Point3f(0.0f, len, 0.0f));
			axisX.add(new Point3f(-dif, len - dif, 0.0f));
			if (!is2d) {
				axisX.add(new Point3f(0.0f, len, 0.0f));
				axisX.add(new Point3f(0.0f, len - dif, dif));
				axisX.add(new Point3f(0.0f, len, 0.0f));
				axisX.add(new Point3f(0.0f, len - dif, -dif));
				axisX.add(new Point3f(0.0f, 0.0f, 0.0f));
				axisX.add(new Point3f(0.0f, 0.0f, len));
				axisX.add(new Point3f(0.0f, 0.0f, len));
				axisX.add(new Point3f(dif, 0.0f, len - dif));
				axisX.add(new Point3f(0.0f, 0.0f, len));
				axisX.add(new Point3f(-dif, 0.0f, len - dif));
				axisX.add(new Point3f(0.0f, 0.0f, len));
				axisX.add(new Point3f(0.0f, dif, len - dif));
				axisX.add(new Point3f(0.0f, 0.0f, len));
				axisX.add(new Point3f(0.0f, -dif, len - dif));
			}
			final GeometryArray axisGeom = new LineArray(30, GeometryArray.COORDINATES
					| GeometryArray.COLOR_3);
			for (int i = 0; i < axisX.size(); i++) {
				axisGeom.setCoordinate(i, axisX.get(i));
			}
			final List <Color3f> colors = new ArrayList <Color3f>();
			final float clbr = 0.7f;
			if (!is2d) {
				for (int i = 0; i < 10; i++) {
					colors.add(new Color3f(0.0f, clbr, clbr));
				}
				for (int i = 10; i < 20; i++) {
					colors.add(new Color3f(clbr, 0.0f, clbr));
				}
				for (int i = 20; i < 30; i++) {
					colors.add(new Color3f(clbr, clbr, 0.0f));
				}
			} else {
				for (int i = 0; i < 6; i++) {
					colors.add(new Color3f(0.0f, clbr, clbr));
				}
				for (int i = 6; i < 12; i++) {
					colors.add(new Color3f(clbr, 0.0f, clbr));
				}
			}
			axisGeom.setColors(0, colors.toArray(new Color3f[0]));
			return axisGeom;
		}
		
		private Geometry createLinesAllVsAll(final List <Point3d> pts, final Color color) {
			final int ptsCount = pts.size();
			// Sum(1 to n)
			final int sum = ptsCount * (ptsCount - 1) / 2;
			final GeometryArray lineArr = new LineArray(sum * 2, GeometryArray.COORDINATES
					| GeometryArray.COLOR_3);
			if (color != null) {
				final Color3f[] colors = new Color3f[sum * 2];
				Arrays.fill(colors, new Color3f(color));
				lineArr.setColors(0, colors);
			}
			int counter = 0;
			for (int i = 0; i < ptsCount - 1; i++) {
				for (int j = i + 1; j < ptsCount; j++) {
					lineArr.setCoordinate(counter++, pts.get(i));
					lineArr.setCoordinate(counter++, pts.get(j));
				}
			}
			return lineArr;
		}
		
		private Geometry createTriangles(final List <Point3d> pts, final Color color) {
			final GeometryArray lineArr = new LineArray(pts.size(), GeometryArray.COORDINATES
					| GeometryArray.COLOR_3);
			for (int i = 0; i < pts.size(); i++) {
				lineArr.setCoordinate(i, pts.get(i));
			}
			if (color != null) {
				final Color3f[] colors = new Color3f[pts.size()];
				Arrays.fill(colors, new Color3f(Color.BLUE));
				lineArr.setColors(0, colors);
			}
			return lineArr;
		}
	}
	
	public static void main(final String[] args) {
		final List <Point3d> points = new ArrayList <Point3d>();
		
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, 3.5, 3.5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, -3.5, 3.5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, 3.5, -3.5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, -3.5, -3.5));
		//
		final boolean is2d = false;
		final Frame frame = new MainFrame(new PolyRenderer(points, TRIANGLES, is2d), 512, 512);
		frame.setVisible(true);
	}
}
