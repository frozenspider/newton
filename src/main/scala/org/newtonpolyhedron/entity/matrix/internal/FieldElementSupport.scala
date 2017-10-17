package org.newtonpolyhedron.entity.matrix.internal

import org.apache.commons.math3.Field
import org.apache.commons.math3.FieldElement
import org.newtonpolyhedron.math.MathImports._

import spire.implicits._
import spire.math.Fractional
import spire.math.Integral
import spire.math.Numeric
import spire.math.Rational

/**
 * Support for Apache Commons Math3's approach to fields, but using type classes instead.
 * This allows us to use FieldElement for types which weren't implementing FieldElement initially.
 * This was necessary because e.g. standard BigInt will can't implement FieldElement, not without reflection.
 *
 * @author FS
 */
private[matrix] object FieldElementSupport {

  type FieldWrapped[T] = Field[FieldElementWrapping[T]]

  // Main wrappers
  val BigIntFieldWrapper: FieldElementWrapper[BigInt] = new FieldElementWrapper
  val RationalFieldWrapper: FieldElementWrapper[Rational] = new FieldElementWrapper
  def mpNumberFieldWrapper[N <: MPNumber: MathProcessor]: FieldElementWrapper[N] = new FieldElementWrapper

  // Wrapping
  def wrap[T: Numeric] = new FieldElementWrapper[T]

  class FieldElementWrapper[T](implicit val numeric: Numeric[T]) {
    implicit val field: FieldWrapped[T] = new Field[FieldElementWrapping[T]] {
      override lazy val getZero = wrappedZero
      override lazy val getOne = wrappedOne
      override def getRuntimeClass() = classOf[FieldElementWrapping[T]]
    }
    lazy val wrappedZero = FieldElementWrapping(numeric.zero)
    lazy val wrappedOne = FieldElementWrapping(numeric.one)
    implicit def apply(a: T): FieldElementWrapping[T] = FieldElementWrapping(a)
  }

  case class FieldElementWrapping[T](val pure: T)(implicit numeric: Numeric[T], field: FieldWrapped[T]) extends FieldElement[FieldElementWrapping[T]] {

    override def add(b: FieldElementWrapping[T]) =
      FieldElementWrapping(pure + b.pure)

    override def subtract(b: FieldElementWrapping[T]) =
      FieldElementWrapping(pure - b.pure)

    override def negate() =
      FieldElementWrapping(-pure)

    override def multiply(n: Int) =
      FieldElementWrapping(pure * n)

    override def multiply(b: FieldElementWrapping[T]) =
      FieldElementWrapping(pure * b.pure)

    override def divide(b: FieldElementWrapping[T]) =
      numeric match {
        case fractional: Fractional[T] =>
          FieldElementWrapping(fractional.div(pure, b.pure))
        case integral: Integral[T] =>
          implicit val _integral = integral
          val (quot, rem) = pure /% b.pure
          if (rem != numeric.zero) {
            throw new UnsupportedOperationException("Non-integral division not supported for this integral type")
          } else {
            FieldElementWrapping(integral.quot(pure, b.pure))
          }
        case other =>
          FieldElementWrapping(numeric.div(pure, b.pure))
      }

    override def reciprocal() =
      throw new UnsupportedOperationException("Reciprocal not supported")

    override def getField(): Field[FieldElementWrapping[T]] =
      field

    override def toString = "Wrapping(" + pure + ")"
  }
}
