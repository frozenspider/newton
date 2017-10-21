package org.newtonpolyhedron.solve.changevars

import scala.collection.IndexedSeq

import org.newtonpolyhedron.NewtonImports._

import spire.compat._

class ChangerOfVariablesImpl[N <: MPNumber](implicit mp: MathProcessor[N]) extends ChangerOfVariables[N] {
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

  // FIXME: Change vars and banish together
  override def changeVars(poly: Polynomial[N], substs: Polys[N]): Polynomial[N] = {
    require(poly forall (_.powers.size == substs.size), "Original polynomial terms dimension should be" +
      "equal to replacement polynomials count")
    val changedVarsPolyWithDup = poly.par flatMap changeVarInTerm(substs)
    val changedVarsPoly: Polynomial[N] = changedVarsPolyWithDup.toIndexedSeq.collapseDups
    changedVarsPoly.sorted
  }

  def changeVarInTerm(substs: Polys[N])(term: Term[N]): Polynomial[N] = {
    assert(substs.size == term.powers.size)
    val powered = (substs zip term.powers).par map {
      case (changeVarPoly, pow) => {
        require(pow.isIntegral, "Power is not a valid integer!")
        changeVarPoly ** pow.intValue
      }
    } map (_.collapseDups) filter (_.size > 0)
    val crossMultiplied = powered reduceLeft (_ * _)
    // Mutiply by original term coefficient
    val scaled = crossMultiplied map (t => t withCoeff (t.coeff * term.coeff))
    scaled
  }
}
