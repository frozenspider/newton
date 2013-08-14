package org.newtonpolyhedron.entity

import scala.collection.immutable.SortedSet
import scala.math.Ordered

class Surface(val pointIndices: SortedSet[Int], val upperSurfaces: IndexedSeq[Surface])
    extends Ordered[Surface] {
  def this(pointIndices: Seq[Int], upperSurfaces: Seq[Surface]) =
    this(SortedSet(pointIndices: _*), upperSurfaces.toIndexedSeq)

  def this(pointIndices: Seq[Int]) =
    this(pointIndices, IndexedSeq.empty)

  val size = pointIndices.size

  def addUpperSurfaces(upperSurfaces: Traversable[Surface]): Surface =
    new Surface(pointIndices, (this.upperSurfaces ++ upperSurfaces).distinct.sorted)

  def makeString(allUpperSurfaces: IndexedSeq[Surface]) = {
    val result = new StringBuilder(pointIndices.mkString("{", ", ", "}"))
    if (!upperSurfaces.isEmpty) {
      result.append(" / ")
      val upperSurfacesIndices = upperSurfaces map (allUpperSurfaces indexOf _)
      assert(upperSurfacesIndices forall (_ != -1),
        "Upper dimension surface is missing  from its upper dimension surfaces list: " +
          allUpperSurfaces(upperSurfacesIndices indexOf -1) +
          " given " + allUpperSurfaces)
      result.append(upperSurfacesIndices.mkString(", "))
    }
    result
  }

  //
  // Standard
  //
  override def equals(that: Any) = that match {
    case that: Surface => this.pointIndices == that.pointIndices && this.upperSurfaces == that.upperSurfaces
    case _             => false
  }

  override lazy val hashCode: Int =
    this.pointIndices.hashCode * 17 + this.upperSurfaces.hashCode

  override lazy val toString =
    "{" + pointIndices.mkString(", ") + "}/" + (upperSurfaces map (_.pointIndices mkString ("(", ", ", ")"))).mkString("[", ", ", "]")

  override def compare(that: Surface): Int = {
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