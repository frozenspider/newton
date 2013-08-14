package org
import java.math.BigInteger
import scala.Numeric.Implicits._
import scala.Ordering.Implicits._
import scala.collection.JavaConversions._
import org.newtonpolyhedron.entity.BigIntFielded
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.vector._
import scala.collection.immutable.SortedSet

// TODO: Split to Scala-Java conversions and other
package object newtonpolyhedron {

  type Polynomial = IndexedSeq[Term]
  type Polys = IndexedSeq[Polynomial]

  implicit def set2sorted[T <: Ordered[T]](t: Set[T]): SortedSet[T] = {
    SortedSet.empty[T] ++ t
  }

  // BigInt conversion
  implicit def int2Fielded(bigInt: BigInt): BigIntFielded =
    new BigIntFielded(bigInt.underlying)

  implicit def int2Fielded(bigInt: BigInteger): BigIntFielded =
    new BigIntFielded(bigInt)

  implicit def int2Fielded(int: Int): BigIntFielded =
    BigIntFielded(int)
}