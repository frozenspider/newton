package org.newtonpolyhedron.utils

import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.collection.JavaConversions._

import org.apache.commons.math3.fraction.BigFraction
import org.apache.commons.math3.fraction.BigFractionField
import org.apache.commons.math3.linear.FieldMatrix
import org.fs.utils.collection.list.SortedArrayList
import org.fs.utils.collection.set.IndexedSet
import org.fs.utils.structure.wrap.Pair
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.vector.VectorImports._

object ScalaJavaConversionUtils {

  // Collections conversion
  implicit def coll2seq[T](coll: java.util.Collection[T]): IndexedSeq[T] =
    if (coll != null) collectionAsScalaIterable(coll).toIndexedSeq
    else Vector.empty

  implicit def set2immset[T](set: java.util.Set[T]): Set[T] =
    if (set != null) asScalaSet(set).toSet
    else Set.empty

  def seq2list[T](vec: Seq[T]): java.util.List[T] =
    seqAsJavaList(vec)

  def immset2indexed[T](set: Set[T]): IndexedSet[T] =
    new SortedArrayList[T](seqAsJavaList(set.toList))

  implicit def convertPair[T1, T2](pair: Pair[T1, T2]) = (pair.getFirst, pair.getSecond)

  implicit def convertPair[T1, T2](pair: (T1, T2)) = Pair.make(pair._1, pair._2)
}
