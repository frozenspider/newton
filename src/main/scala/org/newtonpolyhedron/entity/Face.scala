package org.newtonpolyhedron.entity

import scala.collection.immutable.SortedSet
import scala.math.Ordered

/**
 * Represents N-dimensional face.
 *
 * @author FS
 */
class Face(val pointIndices: SortedSet[Int], val upperFaces: IndexedSeq[Face])
    extends Ordered[Face] {
  def this(pointIndices: Seq[Int], upperFaces: Seq[Face]) =
    this(SortedSet(pointIndices: _*), upperFaces.toIndexedSeq)

  def this(pointIndices: Seq[Int]) =
    this(pointIndices, IndexedSeq.empty)

  val size = pointIndices.size

  def addUpperFaces(upperFaces: Traversable[Face]): Face =
    new Face(pointIndices, (this.upperFaces ++ upperFaces).distinct.sorted)

  def makeString(allUpperFaces: IndexedSeq[Face]) = {
    val result = new StringBuilder(pointIndices.mkString("{", ", ", "}"))
    if (!upperFaces.isEmpty) {
      result.append(" / ")
      val upperFacesIndices = upperFaces map (allUpperFaces indexOf _)
      assert(
        upperFacesIndices forall (_ != -1),
        "Upper dimension face is missing  from its upper dimension faces list: " +
          allUpperFaces(upperFacesIndices indexOf -1) +
          " given " + allUpperFaces
      )
      result.append(upperFacesIndices.mkString(", "))
    }
    result
  }

  //
  // Standard
  //

  override def equals(that: Any) = that match {
    case that: Face => this.pointIndices == that.pointIndices && this.upperFaces == that.upperFaces
    case _             => false
  }

  override lazy val hashCode: Int =
    this.pointIndices.hashCode * 17 + this.upperFaces.hashCode

  override lazy val toString =
    "{" + pointIndices.mkString(", ") + "}/" + (upperFaces map (_.pointIndices mkString ("(", ", ", ")"))).mkString("[", ", ", "]")

  override def compare(that: Face): Int = {
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
