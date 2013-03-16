package org.newtonpolyhedron.entity.vector

abstract case class MathVector[T, SELF <: MathVector[T, SELF]](val elements: IndexedSeq[T])(
  implicit val numeric: Numeric[T],
  implicit val ordering: Ordering[IndexedSeq[T]])
    extends Ordered[SELF]
    with Function1[Int, T]
    with Serializable {

  val dim = elements.size
  lazy val sum = elements.sum
  lazy val max = elements.max
  lazy val head = elements.head
  lazy val tail = create(elements.tail)

  protected def create(elements: IndexedSeq[T]): SELF

  def +(that: SELF) = {
    require(this.dim == that.dim, "Dimension of other vector was different")
    import numeric._
    create((this.elements zip that.elements) map { case (x, y) => x + y })
  }

  def -(that: SELF) = {
    require(this.dim == that.dim, "Dimension of other vector was different")
    import numeric._
    create((this.elements zip that.elements) map { case (x, y) => x - y })
  }

  def *(that: SELF) = {
    require(this.dim == that.dim, "Dimension of other vector was different")
    import numeric._
    create((this.elements zip that.elements) map { case (x, y) => x * y })
  }

  def *(that: Int): SELF =
    this * BigInt(that)

  def *(that: BigInt): SELF

  /** Dot-product */
  def *+(that: SELF) = {
    require(this.dim == that.dim, "Dimension of other vector was different")
    (this * that).sum
  }

  def unary_- = {
    import numeric._
    create(this.elements map { x => -x })
  }

  def updated(index: Int, value: T) =
    create(elements.updated(index, value))

  def apply(index: Int) = elements(index)

  override def compare(that: SELF): Int = {
    import ordering._
    ordering.compare(this.elements, that.elements)
  }

  override def toString = elements.mkString("[ ", " ", " ]")
}
