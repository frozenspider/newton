package org.newtonpolyhedron.solve.changevars

import scala.collection.IndexedSeq

import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.utils.LanguageImplicits._
import org.newtonpolyhedron.utils.PolynomialUtils._

class ChangerOfVariablesImpl extends ChangerOfVariables {
  private val s = IndexedSeq

  //  val lowerstPowersToTake = 4
  //
  //  def pickLowestPowers(p: Poly): Poly = {
  //      // For some reason, the following code causes solutions to be lost
  //      //    def recurse(rem: Poly, acc: Poly, prevPowSum: BigInt, incremets: Int): Poly = {
  //      //      if (rem.isEmpty || incremets == lowerstPowersToTake) acc
  //      //      else {
  //      //        val next = rem.head
  //      //        val nextPowSum = next._2.sum
  //      //        if (prevPowSum == nextPowSum)
  //      //          recurse(rem.tail, acc :+ next, prevPowSum, incremets)
  //      //        else
  //      //          recurse(rem.tail, acc :+ next, nextPowSum, incremets + 1)
  //      //      }
  //      //    }
  //      //    // Sort by powers sum in descending order, skip those with zero coeff
  //      //    val sorted = p sortBy (_._2.sum)
  //      //    val res = recurse(sorted.tail, s(sorted.head), sorted.head._2.sum, 1)
  //      //    res
  //      p sortBy (_.powers.sum)
  //    }

  def changeVars(poly: Polynomial, substs: Polys): Polynomial = {
    require(poly forall (_.powers.size == substs.size), "Original polynomial terms dimension should be" +
      "equal to replacement polynomials count")
    val changedVarsPolyWithDup: Polynomial = poly flatMap changeVarInTerm(substs)
    val changedVarsPoly: Polynomial = changedVarsPolyWithDup.collapseDups
    changedVarsPoly sorted
  }

  def changeVarInTerm(substs: Polys)(term: Term): Polynomial = {
    assert(substs.size == term.powers.size)
    val powered: Polys = (substs zip term.powers) map {
      case (changeVarPoly, pow) => {
        require(pow.isValidInt, "Power is either too large or fractional!")
        changeVarPoly pow pow.toInt
      }
    } map (_.collapseDups) filter (_.size > 0)
    val crossMultiplied = powered reduceLeft (_ * _)
    // Mutiply by original term coefficient
    val scaled = crossMultiplied map (t => t withCoeff (t.coeff * term.coeff))
    scaled
  }
}