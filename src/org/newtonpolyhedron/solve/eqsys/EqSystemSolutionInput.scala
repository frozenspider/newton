package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.utils.PolynomialUtils.Polys
import org.newtonpolyhedron.conversion.latex.LatexString

trait EqSystemSolutionInput {
  def varName: String

  def getInputFor(system: Polys,
                  initialValuesOption: Option[Seq[String]],
                  headerTextOption: Option[LatexString]): Option[Seq[String]]
}
