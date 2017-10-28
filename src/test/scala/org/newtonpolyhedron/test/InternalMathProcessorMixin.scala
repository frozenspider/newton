package org.newtonpolyhedron.test

import org.newtonpolyhedron.entity.math.MathProcessor
import org.newtonpolyhedron.math.internal.InternalMathProcessor

trait InternalMathProcessorMixin {
  type N = org.newtonpolyhedron.math.internal.Product
  implicit val mp: MathProcessor[N] = InternalMathProcessorMixin.mp
}

object InternalMathProcessorMixin {
  private val mp = new InternalMathProcessor
}
