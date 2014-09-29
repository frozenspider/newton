package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.utils.PolynomialUtils.Polys

trait EqSystemSolutionInput {
  def getInputFor(system: Polys, headerTextOption: Option[String]): Option[Seq[String]]
}
