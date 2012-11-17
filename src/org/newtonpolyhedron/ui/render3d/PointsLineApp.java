package org.newtonpolyhedron.ui.render3d;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GraphicsConfiguration;
import java.util.ArrayList;
import java.util.List;

import javax.media.j3d.*;
import javax.swing.JApplet;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Point3f;

import com.sun.j3d.utils.applet.MainFrame;
import com.sun.j3d.utils.universe.SimpleUniverse;

@SuppressWarnings("serial")
public class PointsLineApp extends JApplet {
	
	public static final int	ALL_VS_ALL	= 0;
	public static final int	TRIANGLES	= 1;
	private final boolean	is2d;
	
	// Create a simple scene and attach it to the virtual universe
	public PointsLineApp(final List <Point3d> p, final int mode, final boolean is2d) {
		this.is2d = is2d;
		final GraphicsConfiguration config = SimpleUniverse.getPreferredConfiguration();
		final Canvas3D canvas3D = new Canvas3D(config);
		setLayout(new BorderLayout());
		add("Center", canvas3D);
		final BranchGroup scene = createSceneGraph(p, mode);
		// SimpleUniverse is a Convenience Utility class
		final SimpleUniverse simpleU = new SimpleUniverse(canvas3D);
		// This will move the ViewPlatform back a bit so the objects in the scene can be viewed.
		simpleU.getViewingPlatform().setNominalViewingTransform();
		simpleU.addBranchGraph(scene);
	} // end of YoyoLineApp constructor
	
	public class PointDrawer extends Shape3D {
		
		// //////////////////////////////////////////
		//
		// create Shape3D with geometry and appearance
		// the geometry is created in method yoyoGeometry
		// the appearance is created in method yoyoAppearance
		//
		public PointDrawer(final List <Point3d> p, final int mode) {
			this.setGeometry(axisLines());
			switch (mode) {
				case ALL_VS_ALL:
					this.addGeometry(linesAllVsAll(p, null));
					break;
				case TRIANGLES:
					this.addGeometry(triangles(p, Color.BLUE));
					break;
			}
			this.setAppearance(pointsAppearance());
		} // end of drawPoints constructor
		
		private Geometry axisLines() {
			LineArray axisX;
			final float len = 6.5f;
			final float dif = 0.2f;
			int i = 0;
			axisX = new LineArray(30, GeometryArray.COORDINATES | GeometryArray.COLOR_3);
			axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, 0.0f));
			axisX.setCoordinate(i++, new Point3f(len, 0.0f, 0.0f));
			axisX.setCoordinate(i++, new Point3f(len, 0.0f, 0.0f));
			axisX.setCoordinate(i++, new Point3f(len - dif, dif, 0.0f));
			axisX.setCoordinate(i++, new Point3f(len, 0.0f, 0.0f));
			axisX.setCoordinate(i++, new Point3f(len - dif, -dif, 0.0f));
			if (!is2d) {
				axisX.setCoordinate(i++, new Point3f(len, 0.0f, 0.0f));
				axisX.setCoordinate(i++, new Point3f(len - dif, 0.0f, dif));
				axisX.setCoordinate(i++, new Point3f(len, 0.0f, 0.0f));
				axisX.setCoordinate(i++, new Point3f(len - dif, 0.0f, -dif));
			}
			axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, 0.0f));
			axisX.setCoordinate(i++, new Point3f(0.0f, len, 0.0f));
			axisX.setCoordinate(i++, new Point3f(0.0f, len, 0.0f));
			axisX.setCoordinate(i++, new Point3f(dif, len - dif, 0.0f));
			axisX.setCoordinate(i++, new Point3f(0.0f, len, 0.0f));
			axisX.setCoordinate(i++, new Point3f(-dif, len - dif, 0.0f));
			if (!is2d) {
				axisX.setCoordinate(i++, new Point3f(0.0f, len, 0.0f));
				axisX.setCoordinate(i++, new Point3f(0.0f, len - dif, dif));
				axisX.setCoordinate(i++, new Point3f(0.0f, len, 0.0f));
				axisX.setCoordinate(i++, new Point3f(0.0f, len - dif, -dif));
				axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, 0.0f));
				axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, len));
				axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, len));
				axisX.setCoordinate(i++, new Point3f(dif, 0.0f, len - dif));
				axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, len));
				axisX.setCoordinate(i++, new Point3f(-dif, 0.0f, len - dif));
				axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, len));
				axisX.setCoordinate(i++, new Point3f(0.0f, dif, len - dif));
				axisX.setCoordinate(i++, new Point3f(0.0f, 0.0f, len));
				axisX.setCoordinate(i++, new Point3f(0.0f, -dif, len - dif));
			}
			Color3f[] colors;
			final float clbr = 0.7f;
			if (!is2d) {
				colors = new Color3f[30];
				for (i = 0; i < 10; i++) {
					colors[i] = new Color3f(0.0f, clbr, clbr);
				}
				for (i = 10; i < 20; i++) {
					colors[i] = new Color3f(clbr, 0.0f, clbr);
				}
				for (i = 20; i < 30; i++) {
					colors[i] = new Color3f(clbr, clbr, 0.0f);
				}
			} else {
				colors = new Color3f[12];
				for (i = 0; i < 6; i++) {
					colors[i] = new Color3f(0.0f, clbr, clbr);
				}
				for (i = 6; i < 12; i++) {
					colors[i] = new Color3f(clbr, 0.0f, clbr);
				}
			}
			axisX.setColors(0, colors);
			// axisX.setColor(1,red);
			return axisX;
		}
		
		private Geometry linesAllVsAll(final List <Point3d> p, final Color highlighColor) {
			final int ps = p.size();
			int sz = 0;
			for (int i = 1; i < ps; i++) {
				sz += i;
			}
			LineArray la = new LineArray(sz * 2, GeometryArray.COORDINATES | GeometryArray.COLOR_3);
			if (highlighColor != null) {
				final Color3f[] colors = new Color3f[sz * 2];
				for (int i = 0; i < sz * 2; i++) {
					colors[i] = new Color3f(highlighColor);
				}
				la.setColors(0, colors);
			}
			sz = 0;
			for (int i = 0; i < ps - 1; i++) {
				final Point3d pT = p.get(i);
				for (int j = i + 1; j < ps; j++) {
					la.setCoordinate(sz++, pT);
					la.setCoordinate(sz++, p.get(j));
				}
			}
			return la;
		}
		
		private Geometry triangles(final List <Point3d> p, final Color color) {
			LineArray la = new LineArray(p.size(), GeometryArray.COORDINATES
					| GeometryArray.COLOR_3);
			final Color3f[] colors = new Color3f[p.size()];
			final Color3f color3f = new Color3f(color);
			for (int i = 0; i < p.size();) {
				colors[i] = color3f;
				la.setCoordinate(i, p.get(i++));
			}
			la.setColors(0, colors);
			return la;
		}
		
		// //////////////////////////////////////////
		//
		// create appearance
		//
		private Appearance pointsAppearance() {
			final Appearance appearance = new Appearance();
			final PolygonAttributes polyAttrib = new PolygonAttributes();
			polyAttrib.setPolygonMode(PolygonAttributes.POLYGON_LINE);
			appearance.setPolygonAttributes(polyAttrib);
			return appearance;
		}
	} // end of class drawPoints
	
	/*-
	 public class drawCoords extends Shape3D{

	 ////////////////////////////////////////////
	 //
	 // create Shape3D with geometry and appearance
	 // the geometry is created in method yoyoGeometry
	 // the appearance is created in method yoyoAppearance
	 //
	 public drawCoords(final ArrayList<Point3d> p, ArrayList<Point3d> bPts, int mode) {
	 this.addGeometry(axisCoords());
	 //    		switch(mode){
	 //    			case 0:
	 ////    				for(int i=0; i<p.size()-1; i++)
	 ////    					for(int j=i+1; j<p.size(); j++)
	 //    						this.addGeometry(pointsGeometry_Lines(p));
	 //    				break;
	 //    			case 2:
	 //    				this.addGeometry(pointsGeometry_Prisms(bPts));
	 //    			case 1:
	 //    				this.addGeometry(pointsGeometry_Triangles(p));
	 //    				break;
	 //    		}
	 //    		this.setAppearance(pointsAppearance());


	 } // end of drawPoints constructor

	 private Geometry axisCoords() {
	 Text3D axisXt;
	 Font3D font3d = new Font3D(new Font("Courier New", Font.PLAIN, 2),
	 new FontExtrusion());

	 axisXt = new Text3D(font3d, new String("TestText"),
	 new Point3f(3.5f, 0.0f, 0.0f));
	 return axisXt;

	 }
	 private Appearance pointsAppearance() {

	 Appearance appearance = new Appearance();
	 PolygonAttributes polyAttrib = new PolygonAttributes();

	 polyAttrib.setPolygonMode(PolygonAttributes.POLYGON_LINE);
	 appearance.setPolygonAttributes(polyAttrib);

	 return appearance;

	 }
	 }
	 //////////////////////////////////////////
	 */
	// ///////////////////////////////////////////////
	//
	// create scene graph branch group
	//
	public BranchGroup createSceneGraph(final List <Point3d> p, final int mode) {
		final BranchGroup objRoot = new BranchGroup();
		/*
		 * Create the transform group node and initialize it to the identity. Enable the
		 * TRANSFORM_WRITE capability so that our behavior code can modify it at runtime. Add it to
		 * the root of the subgraph.
		 */
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
		// Text2D temt_text
		// = new Text2D("Testing text", new Color3f(1.0f, 1.0f, 0.0f), "Courier New",18,0);
		// objRoot.addChild(temt_text);
		objRoot.addChild(objScale);
		objScale.addChild(objRotate);
		objRotate.addChild(objSpin);
		objSpin.addChild(new PointDrawer(p, mode));
		/*
		 * Create a new Behavior object that will perform the desired operation on the specified
		 * transform object and add it into the scene graph.
		 */
		final Transform3D zAxis = new Transform3D();
		zAxis.rotX(Math.PI / 2.0d);
		final Alpha rotationAlpha = new Alpha(-1, 20000);
		final RotationInterpolator rotator = new RotationInterpolator(rotationAlpha, objSpin,
				zAxis, 0.0f, (float) Math.PI * 2.0f);
		final BoundingSphere bounds = new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);
		rotator.setSchedulingBounds(bounds);
		/*-      ///////////////////////////////////////////////
		 Vector3f translate = new Vector3f();
		 Transform3D T3D = new Transform3D();
		 TransformGroup TGR = new TransformGroup();
		 Billboard billboard = null;
		 //    BoundingSphere bSphere = new BoundingSphere();

		 translate.set(new Point3f(1.0f, 1.0f, 0.0f));

		 // set up for billboard behavior
		 TGR.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
		 billboard = new Billboard(TGR);
		 billboard.setSchedulingBounds(bounds);

		 objSpin.addChild(TGR);
		 objSpin.addChild(billboard);
		 Node dC = new drawCoords(p, bPts, mode);
		 TGR.addChild(dC);
		 //    objSpin.addChild(new drawCoords(p, bPts, mode));
		 ///////////////////////////////////////////////
		 */
		final Background bg_white = new Background(new Color3f(0.9f, 0.9f, 0.9f));
		bg_white.setApplicationBounds(bounds);
		objRoot.addChild(bg_white);
		if (!is2d) {
			objSpin.addChild(rotator);
		}
		// Let Java 3D perform optimizations on this scene graph.
		objRoot.compile();
		return objRoot;
	}
	
	public static Frame doDrawFrame(
			final List <Point3d> p,
			final int mode,
			final int x,
			final int y,
			final int w,
			final int h,
			final boolean l_d_2d) {
		// Frame frame = new MainFrame(new PointsLineApp(p,center,mode,l_d_2d), w, h);
		final PointsLineApp a = new PointsLineApp(p, mode, l_d_2d);
		final MainFrame frame = new MainFrame(a, w, h);
		frame.setLocation(x, y);
		return frame;
	}
	
	public static void main(final String[] args) {
		final ArrayList <Point3d> points = new ArrayList <Point3d>();
		// points.add(new Point3d(0.0, 0.0, 0.0));
		// points.add(new Point3d(0.0, 0.0, 3.0));
		// points.add(new Point3d(2.0, 0.0, 0.0));
		// points.add(new Point3d(-2.0, 0.0, 0.0));
		// points.add(new Point3d(0.0, 1.0, 0.0));
		// points.add(new Point3d(0.0, -1.0, 0.0));
		// points.add(new Point3d( 2.015952008357342, 3.043692247912065, 0.0d ));
		// points.add(new Point3d( 2.015952008357342 ,-0.11858541225631414, 0.0d ));
		// points.add(new Point3d( 0.75104094428999, 0.5138701197773616, 0.0d ));
		// points.add(new Point3d( -0.8300978857941996, 2.095008949861551, 0.0d ));
		// points.add(new Point3d( 0.43481317827315213 ,-1.6997242423405037, 0.0d ));
		// points.add(new Point3d( -0.8300978857941994, -1.067268710306828, 0.0d ));
		// points.add(new Point3d( -2.095008949861551, -0.4348131782731522, 0.0d ));
		// points.add(new Point3d( -1.4625534178278756, -2.33217977437418, 0.0d ));
		// points.add(new Point3d(0.3394791575920017, -4.242640687119285, -2.4004801440480166));
		// points.add(new Point3d(0.33947915759200237, 1.414213562373095, -2.4004801440480166));
		// points.add(new Point3d(0.33947915759200215, -1.4142135623730951, -2.4004801440480166));
		// points.add(new Point3d(0.33947915759200237, 4.242640687119285, -2.4004801440480166));
		// points.add(new Point3d(-1.4427864197660114, 2.220446049250313E-16, 3.0606121836612217));
		// points.add(new Point3d(0.33947915759200276, 2.82842712474619, 4.740948284494833));
		// points.add(new Point3d(-0.2546093681940023, -2.82842712474619, 1.8003601080360128));
		// points.add(new Point3d(-0.2546093681940023, -2.82842712474619, 1.8003601080360128));
		// points.add(new Point3d(0, -4.242640687119285, -2.4243661069253055));
		// points.add(new Point3d(0, 1.4142135623730951, -2.4243661069253055));
		// points.add(new Point3d(0, -1.414213562373095, -2.4243661069253055));
		// points.add(new Point3d(0, 4.242640687119285, -2.4243661069253055));
		// points.add(new Point3d(-1.0000000000000004, 0, 3.2324881425670746));
		// points.add(new Point3d(1, 2.82842712474619, 4.6467017049401695));
		// points.add(new Point3d(0, -2.82842712474619, 1.8182745801939795));
		// points.add(new Point3d(3.5264679880282825, 3.4499345241279276,0.0d));
		// points.add(new Point3d(0.2279834875341529, 0.9760711487573314,0.0d));
		// points.add(new Point3d(-1.6152872627419783, 0.19995714864106628,0.0d));
		// points.add(new Point3d(0.06790997501017315, 0.5831634361984721,0.0d));
		// points.add(new Point3d(-3.0705010129599764, -1.4977922266132637,0.0d));
		// points.add(new Point3d(0.6160404875922854, 0.054435773619265715,0.0d));
		// points.add(new Point3d(1.004097487650418, -0.8671996015187997,0.0d));
		// points.add(new Point3d(0.08246211251235241, -1.2552566015769324,0.0d));
		// points.add(new Point3d(-0.8391732626257131, -1.643313601635065,0.0d));
		// points.add(new Point3d(-0.8391732626257131, -1.643313601635065,0.0d));
		// points.add(new Point3d(0.6440668264853728, -4.891135104899381,0.0d));
		// points.add(new Point3d(0.6440668264853734, -0.7680294792817209,0.0d));
		// points.add(new Point3d(1.1291380765580397, 1.172255521008943,0.0d));
		// points.add(new Point3d(0.42578476395267384, -0.40422604172722165,0.0d));
		// points.add(new Point3d(0.6440668264853739, 3.3550761463359398,0.0d));
		// points.add(new Point3d(-0.32607567365995854, -0.5254938542453879,0.0d));
		// points.add(new Point3d(-1.2962181738052905, -0.2829582292090547,0.0d));
		// points.add(new Point3d(-1.0536825487689572, 0.6871842709362772,0.0d));
		// points.add(new Point3d(-0.8111469237326241, 1.657326771081609,0.0d));
		/*
		 * [-1.66 1 0.6] [1.66 1 0.6] [0 -2 0.5] [0 0 -2]
		 */
		// points.add(new Point3d(-5, 0, 0));
		// points.add(new Point3d(0, 5, 0));
		// points.add(new Point3d(0, -5, 0));
		// points.add(new Point3d(0, 0, 5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, 3.5, 3.5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, -3.5, 3.5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, 3.5, -3.5));
		points.add(new Point3d(5, 0, 0));
		points.add(new Point3d(0, -3.5, -3.5));
		//
		final boolean l_d_2d = false;
		// points.add(new Point3d(-1.2781576242108297, -0.06893679191721644, -1.057244835532626));
		// points.add(new Point3d(0.3275930944910817, -0.10774888164628657, -0.1520350244684631));
		// points.add(new Point3d(0.3275930944910817, 0.32852034164502464, 0.4635463256533068));
		// points.add(new Point3d(0.6229714352286659, -0.1518346680815222, 0.7457335343477829));
		//
		// double x1=0.0d, y1=0.0d, z1=0.0d;
		// for(int i=0; i<points.size(); i++){
		// x1+=points.get(i).x;
		// y1+=points.get(i).y;
		// z1+=points.get(i).z;
		// }
		// x1 = x1/points.size();
		// y1 = y1/points.size();
		// z1 = z1/points.size();
		// points.add(new Point3d(x1,y1,z1));
		// double[] x = {points.get(0).x, points.get(1).x, points.get(2).x};
		// double[] y = {points.get(0).y, points.get(1).y, points.get(2).y};
		// double[] z = {points.get(0).z, points.get(1).z, points.get(2).z};
		// double A = (y[1]-y[0])*(z[2]-z[0]) - (y[2]-y[0])*(z[1]-z[0]);
		// double B = (x[1]-x[0])*(z[2]-z[0]) - (x[2]-x[0])*(z[1]-z[0]);
		// double C = (x[1]-x[0])*(y[2]-y[0]) - (x[2]-x[0])*(y[1]-y[0]);
		// double t = (A*x[0] - B*y[0] + C*z[0])/(A*A + B*B + C*C);
		// Point3d pMid = new Point3d(A*t,-B*t,C*t);
		// double[] x = {points.get(0).x, points.get(1).x};
		// double[] y = {points.get(0).y, points.get(1).y};
		// double[] z = {points.get(0).z, points.get(1).z};
		// double L = x[1]-x[0];
		// double M = y[1]-y[0];
		// double N = z[1]-z[0];
		// double t = -(L*x[0] + M*y[0] + N*z[0])/(L*L + M*M + N*N);
		// Point3d pMid = new Point3d(L*t + x[0], M*t + y[0], N*t + z[0]);
		// points.add(pMid);
		final Frame frame = new MainFrame(new PointsLineApp(points, TRIANGLES, l_d_2d), 512, 512);
		frame.setVisible(true);
	} // end of main method of YoyoLineApp
} // end of class YoyoLineApp
