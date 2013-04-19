package org.newtonpolyhedron.ui.render3d

import java.awt.Color

import org.newtonpolyhedron.utils.PointUtils._

import javax.media.j3d.Appearance
import javax.media.j3d.Geometry
import javax.media.j3d.GeometryArray._
import javax.media.j3d.LineArray
import javax.media.j3d.PolygonAttributes
import javax.media.j3d.Shape3D
import javax.vecmath.Color3f
import javax.vecmath.Point3d

class PointDrawer extends Shape3D {

  def this(pts: Seq[Point3d],
           mode: Int,
           is2d: Boolean) = {
    this
    setGeometry(createAxisLines(is2d))
    mode match {
      case PolyRenderer.ALL_VS_ALL => addGeometry(createLinesAllVsAll(pts, null))
      case PolyRenderer.TRIANGLES  => addGeometry(createTriangles(pts, Color.BLUE))
    }
    setAppearance(createPointsAppearance)
  }

  def createPointsAppearance = {
    val appearance = new Appearance
    val polyAttrib = new PolygonAttributes
    polyAttrib.setPolygonMode(PolygonAttributes.POLYGON_LINE)
    appearance.setPolygonAttributes(polyAttrib)
    appearance
  }

  def createAxisLines(is2d: Boolean): Geometry = {
    val len = 6.5
    val dif = 0.2
    val z = 0.0
    val axisX = Seq(
      p3d(z, z, z),
      p3d(len, z, z),
      p3d(len, z, z),
      p3d(len - dif, dif, z),
      p3d(len, z, z),
      p3d(len - dif, -dif, z)) ++
      (if (!is2d) Seq(
        p3d(len, z, z),
        p3d(len - dif, z, dif),
        p3d(len, z, z),
        p3d(len - dif, z, -dif))
      else Seq()) ++
      Seq(
        p3d(z, z, z),
        p3d(z, len, z),
        p3d(z, len, z),
        p3d(dif, len - dif, z),
        p3d(z, len, z),
        p3d(-dif, len - dif, z)) ++
        (if (!is2d) Seq(
          p3d(z, len, z),
          p3d(z, len - dif, dif),
          p3d(z, len, z),
          p3d(z, len - dif, -dif),
          p3d(z, z, z),
          p3d(z, z, len),
          p3d(z, z, len),
          p3d(dif, z, len - dif),
          p3d(z, z, len),
          p3d(-dif, z, len - dif),
          p3d(z, z, len),
          p3d(z, dif, len - dif),
          p3d(z, z, len),
          p3d(z, -dif, len - dif))
        else Seq())

    val axisGeom = new LineArray(30, COORDINATES | COLOR_3)
    (0 until axisX.size) map (i => axisGeom setCoordinate (i, axisX(i)))

    def c3f = (x: Double, y: Double, z: Double) => new Color3f(x.toFloat, y.toFloat, z.toFloat)
    val clbr = 0.7
    val colors =
      if (!is2d) {
        Seq(
          c3f(z, clbr, clbr),
          c3f(clbr, z, clbr),
          c3f(clbr, clbr, z)) flatMap (Seq.fill(10)(_))
      } else {
        Seq(
          c3f(z, clbr, clbr),
          c3f(clbr, z, clbr)) flatMap (Seq.fill(06)(_))
      }
    axisGeom.setColors(0, colors.toArray[Color3f])
    axisGeom
  }

  def createLinesAllVsAll(pts: Seq[Point3d], color: Color): Geometry = {
    val sz = pts.size
    val sum = (1 to (sz - 1)).sum
    val lineArr = new LineArray(sum * 2, COORDINATES | COLOR_3)
    if (color != null) {
      lineArr.setColors(0, Array.fill(sum * 2)(new Color3f(color)))
    }
    var counter = 0
    for {
      i <- 0 until sz
      j <- (i + 1) until sz
    } {
      lineArr.setCoordinate(counter, pts(i))
      counter += 1
      lineArr.setCoordinate(counter, pts(j))
      counter += 1
    }
    lineArr
  }

  def createTriangles(pts: Seq[Point3d], color: Color): Geometry = {
    val lineArr = new LineArray(pts.size, COORDINATES | COLOR_3)
    (0 until pts.size) map (i => lineArr.setCoordinate(i, pts(i)))
    if (color != null) {
      val colors = Array.fill(pts.size)(new Color3f(Color.BLUE))
      lineArr.setColors(0, colors)
    }
    return lineArr
  }

}