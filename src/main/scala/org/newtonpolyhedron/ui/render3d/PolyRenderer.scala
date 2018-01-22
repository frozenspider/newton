package org.newtonpolyhedron.ui.render3d

import java.awt.BorderLayout

import scala.math._

import org.newtonpolyhedron.utils.PointUtils._

import com.sun.j3d.utils.applet.MainFrame
import com.sun.j3d.utils.universe.SimpleUniverse

import javax.media.j3d.Alpha
import javax.media.j3d.Background
import javax.media.j3d.BoundingSphere
import javax.media.j3d.BranchGroup
import javax.media.j3d.Canvas3D
import javax.media.j3d.RotationInterpolator
import javax.media.j3d.Transform3D
import javax.media.j3d.TransformGroup
import javax.swing.JApplet
import javax.vecmath.Color3f
import javax.vecmath.Point3d

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
class PolyRenderer(
  pts:  Seq[Point3d],
  mode: Int,
  is2d: Boolean
) extends JApplet {

  private val canvas3D = {
    val config = SimpleUniverse.getPreferredConfiguration
    new Canvas3D(config)
  }
  setLayout(new BorderLayout)
  add("Center", canvas3D)

  private val scene = createSceneGraph(pts, mode, is2d)

  /** SimpleUniverse is a convenience utility class */
  private val universe = {
    val u = new SimpleUniverse(canvas3D)
    // This will move the ViewPlatform back a bit so the objects in the scene can be viewed.
    u.getViewingPlatform.setNominalViewingTransform
    u.addBranchGraph(scene)
    u
  }

  /** Create scene graph branch group */
  def createSceneGraph(
    pts:  Seq[Point3d],
    mode: Int,
    is2d: Boolean
  ): BranchGroup = {
    val objRoot = new BranchGroup
    // Create the transform group node and initialize it to the identity.
    // Enable the TRANSFORM_WRITE capability so that our behavior code can modify it at runtime.
    // Add it to the root of the subgraph.
    val rotate = new Transform3D
    if (!is2d) {
      rotate.rotX(Pi * 1.75d)
      val tempRotate = new Transform3D
      tempRotate.rotZ(Pi * 1.15d)
      rotate.mul(tempRotate)
    }
    val objRotate = new TransformGroup(rotate)
    val objSpin = new TransformGroup
    objSpin.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE)
    val scale = new Transform3D
    scale.setScale(0.1d)
    val objScale = new TransformGroup(scale)
    objRoot.addChild(objScale)
    objScale.addChild(objRotate)
    objRotate.addChild(objSpin)
    objSpin.addChild(new PointDrawer(pts, mode, is2d))
    // Text2D testText = new Text2D("Testing text", new Color3f(1.0f, 1.0f, 0.0f),
    // "Courier New", 150, 0)
    // objSpin.addChild(testText)
    /*
		 * Create a new Behavior object that will perform the desired operation on the specified
		 * transform object and add it into the scene graph.
		 */
    val zAxis = new Transform3D
    zAxis.rotX(Pi / 2)
    val rotationAlpha = new Alpha(-1, 20000)
    val bounds = new BoundingSphere(new Point3d(0, 0, 0), 100.0)

    val bgWhite = new Background(new Color3f(0.9f, 0.9f, 0.9f))
    bgWhite.setApplicationBounds(bounds)
    objRoot.addChild(bgWhite)
    if (!is2d) {
      val rotator = new RotationInterpolator(rotationAlpha, objSpin, zAxis, 0f, (Pi * 2f).toFloat)
      rotator.setSchedulingBounds(bounds)
      objSpin.addChild(rotator)
    }
    // Let Java 3D perform optimizations on this scene graph.
    objRoot.compile
    objRoot
  }
}

object PolyRenderer {
  val AllVsAll = 0
  val Triangles = 1

  def main(args: Array[String]): Unit = {
    val points = Seq(
      p3d(5, 0, 0),
      p3d(0, 3.5, 3.5),
      p3d(5, 0, 0),
      p3d(0, -3.5, 3.5),
      p3d(5, 0, 0),
      p3d(0, 3.5, -3.5),
      p3d(5, 0, 0),
      p3d(0, -3.5, -3.5)
    )
    //
    val is2d = false
    val frame = new MainFrame(new PolyRenderer(points, Triangles, is2d), 512, 512)
    frame.setVisible(true)
  }
}
