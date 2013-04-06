package org.newtonpolyhedron.entity

import org.newtonpolyhedron._
import scala.math.Ordered
import scala.collection.immutable.SortedSet

class Surface(val pointIndices: SortedSet[Int], val upperSurfaces: IndexedSeq[Surface])
    extends Ordered[Surface] {
  def this(pointIndices: Seq[Int], upperSurfaces: Seq[Surface]) =
    this(SortedSet(pointIndices: _*), upperSurfaces.toIndexedSeq)

  def this(pointIndices: Seq[Int]) =
    this(pointIndices, IndexedSeq.empty)

  def this(pointIndices: java.util.List[java.lang.Integer], upperSurfaces: java.util.List[Surface]) = {
    this(coll2seq(pointIndices) map (_.intValue), upperSurfaces)
  }

  val size = pointIndices.size

  def addUpperSurfaces(upperSurfaces: Traversable[Surface]): Surface =
    new Surface(pointIndices, (this.upperSurfaces ++ upperSurfaces).distinct.sorted)

  override def equals(that: Any) = that match {
    case that: Surface => this.pointIndices == that.pointIndices && this.upperSurfaces == that.upperSurfaces
    case _             => false
  }

  override lazy val hashCode: Int =
    this.pointIndices.hashCode * 17 + this.upperSurfaces.hashCode

  override lazy val toString =
    "{" + pointIndices.mkString(", ") + "}/" + (upperSurfaces map (_.pointIndices mkString ("(", ", ", ")"))).mkString("[", ", ", "]")

  def makeString(allUpperSurfaces: IndexedSeq[Surface]) = {
    val result = new StringBuilder
    result.append('{');
    result.append(pointIndices.mkString(", "));
    result.append("}");
    if (!upperSurfaces.isEmpty) {
      result.append(" / ");
      val upperSurfacesIndices = upperSurfaces map (allUpperSurfaces.indexOf(_))
      assert(upperSurfacesIndices forall (_ != -1), "Upper dimension surface is missing"
        + " from its all upper dimension surfaces list: " + allUpperSurfaces(upperSurfacesIndices.indexOf(-1)) + " given "
        + allUpperSurfaces)
      result.append(upperSurfacesIndices.mkString(", "))
    }
    result
  }

  def compare(that: Surface): Int = {
    (this.pointIndices.toSeq zipAll (that.pointIndices, -1, -1)) find {
      case (i1, i2) => i1 != i2 || i1 == -1 || i2 == -1
    } match {
      case Some((_, -1))  => 1
      case Some((-1, _))  => -1
      case Some((i1, i2)) => i1 compare i2
      case None           => 0
    }
  }
}