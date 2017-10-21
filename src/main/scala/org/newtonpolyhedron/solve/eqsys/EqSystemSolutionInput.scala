package org.newtonpolyhedron.solve.eqsys

import org.newtonpolyhedron.NewtonImports._
import org.newtonpolyhedron.utils.LatexConversion.LatexString

trait EqSystemSolutionInput[N <: MPNumber] {
  def varName: String

  def getInputFor(
      system:              Polys[N],
      initialValuesOption: Option[Seq[String]],
      headerTextOption:    Option[LatexString]
  ): Option[Seq[String]]
}
