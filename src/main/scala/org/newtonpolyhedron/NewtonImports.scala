package org.newtonpolyhedron

/**
 * All necessary imports summarized in one trait/object.
 *
 * @author FS
 */
trait NewtonImports
    extends org.newtonpolyhedron.utils.LanguageImports
    with org.newtonpolyhedron.math.MathImports
    with org.newtonpolyhedron.math.PolynomialImports
    with org.newtonpolyhedron.entity.vector.VectorImports
    with org.fs.utility.Imports {

  type Term[N <: MPNumber] = org.newtonpolyhedron.entity.Term[N]
  val Term = org.newtonpolyhedron.entity.Term
}

object NewtonImports extends NewtonImports
