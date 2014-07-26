package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.Polys

trait EqSystemSolutionInput {
  def getInputFor(system: Polys, headerTextOption: Option[String]): Option[Seq[String]]
}
