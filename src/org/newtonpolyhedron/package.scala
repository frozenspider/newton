package org

import scala.collection.JavaConversions._
import org.newtonpolyhedron.entity.vector._
import Ordering.Implicits._
import Numeric.Implicits._
import org.apache.commons.math3.fraction.BigFraction
import org.newtonpolyhedron.entity.BigFrac
import org.newtonpolyhedron.entity.vector.FractionVector
import org.newtonpolyhedron.entity.vector.IntVector
import org.apache.commons.math3.linear.FieldMatrix
import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.Field
import org.newtonpolyhedron.entity.BigFrac.BigFracField
import org.apache.commons.math3.fraction.BigFractionField
import org.fs.utils.structure.wrap.Pair
import org.newtonpolyhedron.entity.Matrix

package object newtonpolyhedron {

  // Collections
  implicit def list2seq[T](list: java.util.List[T]) =
    if (list != null) asScalaBuffer(list).toIndexedSeq
    else Vector.empty

  implicit def set2immset[T](set: java.util.Set[T]) =
    if (set != null) asScalaSet(set).toSet
    else Set.empty

  def seq2list[T](vec: Seq[T]): java.util.List[T] = {
    seqAsJavaList(vec)
  }

  implicit def convertPair[T1, T2](pair: Pair[T1, T2]) = (pair.getFirst, pair.getSecond)

  implicit def convertPair[T1, T2](pair: (T1, T2)) = Pair.make(pair._1, pair._2)

  // Vectors
  def intvec2mathvec[T](vec: IntVector): IntMathVec = {
    val content = (vec.getContentCopy() map (x => new BigInt(x))).toIndexedSeq
    new IntMathVec(content)
  }

  def fracvec2mathvec[T](vec: FractionVector): FracMathVec = {
    val content = (vec.getContentCopy() map (x => new BigFrac(x))).toIndexedSeq
    new FracMathVec(content)
  }

  def mathvec2intvec[T](vec: IntMathVec): IntVector = {
    new IntVector(vec.elements map (_.underlying))
  }

  def mathvec2fracvec[T](vec: FracMathVec): FractionVector = {
    new FractionVector(vec.elements map (_.underlying))
  }

  // Matrices
  private def createOfSizeAs[S <: FieldElement[S], T <: FieldElement[T]](src: FieldMatrix[S])(implicit field: Field[T]) =
    org.apache.commons.math3.linear.MatrixUtils.createFieldMatrix(field,
      src.getRowDimension,
      src.getColumnDimension)

  implicit def matrixJava2Scala(src: FieldMatrix[BigFraction]) = {
    val res = createOfSizeAs(src)
    for {
      ir <- 0 until src.getRowDimension
      ic <- 0 until src.getColumnDimension
    } {
      res.setEntry(ir, ic, new BigFrac(src.getEntry(ir, ic)))
    }
    new Matrix(res)
  }

  implicit def matrixScala2Java(src: Matrix[BigFrac]) = {
    val content = src.contentCopy
    val res = createOfSizeAs(content)(BigFractionField.getInstance)
    for {
      ir <- 0 until content.getRowDimension
      ic <- 0 until content.getColumnDimension
    } {
      res.setEntry(ir, ic, content.getEntry(ir, ic).underlying)
    }
    res
  }
}