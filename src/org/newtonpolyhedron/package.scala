package org

import scala.collection.JavaConversions._

package object newtonpolyhedron {
  implicit def list2seq[T](list: java.util.List[T]) =
    if (list != null) asScalaBuffer(list).toIndexedSeq
    else Vector.empty

  implicit def set2immset[T](set: java.util.Set[T]) =
    if (set != null) asScalaSet(set).toSet
    else Set.empty

  def seq2list[T](vec: IndexedSeq[T]): java.util.List[T] = {
    seqAsJavaList(vec)
  }
}