package org

import scala.collection.JavaConversions._
import org.newtonpolyhedron.entity.vector._
import Ordering.Implicits._
import Numeric.Implicits._
import org.apache.commons.math3.fraction.BigFraction
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntVector

package object newtonpolyhedron {

  // Collections
  implicit def list2seq[T](list: java.util.List[T]) =
    if (list != null) asScalaBuffer(list).toIndexedSeq
    else Vector.empty

  implicit def set2immset[T](set: java.util.Set[T]) =
    if (set != null) asScalaSet(set).toSet
    else Set.empty

  def seq2list[T](vec: IndexedSeq[T]): java.util.List[T] = {
    seqAsJavaList(vec)
  }

  // Vectors
  def intvec2mathvec[T](vec: IntVector): IntMathVec = {
    val content = (vec.getContentCopy() map (x => new BigInt(x))).toIndexedSeq
    new IntMathVec(content)
  }

  //  def fracvec2mathvec[T](vec: FractionVector): FracMathVec = {
  //    val content = (vec.getContentCopy() map (x => new BigFrac(x))).toIndexedSeq
  //    new FracMathVec(content)
  //  }

  def mathvec2intvec[T](vec: IntMathVec): IntVector = {
    new IntVector(vec.elements map (_.underlying))
  }

  //  def mathvec2fracvec[T](vec: FracMathVec): FractionVector = {
  //    new FractionVector(vec.elements map (_.underlying))
  //  }
}