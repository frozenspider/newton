package org.newtonpolyhedron.entity.matrix.internal

import org.apache.commons.math3.FieldElement
import org.apache.commons.math3.Field
import org.newtonpolyhedron.entity.BigFrac

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
  val bigIntFieldWrapper: FieldElementWrapper[BigInt] = new FieldElementWrapper
  val bigFracFieldWrapper: FieldElementWrapper[BigFrac] = new FieldElementWrapper

  // Wrapping
  def wrap[T](implicit fractional: Numeric[T]) = new FieldElementWrapper[T]

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
    import numeric._

    override def add(b: FieldElementWrapping[T]) =
      FieldElementWrapping(pure + b.pure)

    override def subtract(b: FieldElementWrapping[T]) =
      FieldElementWrapping(pure - b.pure)

    override def negate() =
      FieldElementWrapping(-pure)

    override def multiply(n: Int) =
      FieldElementWrapping(pure * fromInt(n))

    override def multiply(b: FieldElementWrapping[T]) =
      FieldElementWrapping(pure * b.pure)

    override def divide(b: FieldElementWrapping[T]) =
      numeric match {
        case fractional: Fractional[T] =>
          FieldElementWrapping(fractional.div(pure, b.pure))
        case integral: Integral[T] =>
          import integral._
          val (quot, rem) = pure /% b.pure
          if (rem != numeric.zero) {
            throw new UnsupportedOperationException("Non-integral division not supported for this integral type")
          } else {
            FieldElementWrapping(integral.quot(pure, b.pure))
          }
        case _ =>
          throw new UnsupportedOperationException("Division not supported for this Numeric, where did you get it anyway?")
      }

    override def reciprocal() =
      throw new UnsupportedOperationException("Reciprocal not supported")

    override def getField(): Field[FieldElementWrapping[T]] =
      field

    override def toString = "Wrapping(" + pure + ")"
  }
}