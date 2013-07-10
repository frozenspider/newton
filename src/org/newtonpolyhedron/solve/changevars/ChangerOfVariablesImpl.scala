package org.newtonpolyhedron.solve.changevars

import scala.collection.IndexedSeq
import org.newtonpolyhedron.entity.Term
import org.newtonpolyhedron.entity.Product
import org.newtonpolyhedron.entity.vector.IntMathVec
import scala.collection.immutable.SortedSet

class ChangerOfVariablesImpl extends ChangerOfVariables {
  private val s = IndexedSeq
  private val imv = IntMathVec

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
    require(poly forall (_.powers.dim == substs.size), "Original polynomial terms dimension should be" +
      "equal to replacement polynomials count")
    val changedVarsPolyWithDup: Polynomial = poly flatMap changeVarInTerm(substs)
    val changedVarsPoly: Polynomial = collapseDups(changedVarsPolyWithDup)
    changedVarsPoly sorted
  }

  def changeVarInTerm(substs: Polys)(term: Term): Polynomial = {
    assert(substs.size == term.powers.dim)
    val powered: Polys = (substs zip term.powers.elements) map {
      case (changeVarPoly, pow) => {
        require(pow.isValidInt, "Power is too large!")
        raisePolyToPower(changeVarPoly, pow.toInt)
      }
    } map skipZeroTerms filter (_.size > 0) map collapseDups
    val crossMultiplied = powered reduceLeft multiplyPolys
    // Mutiply by original term coefficient
    val scaled = crossMultiplied map (t => t withCoeff (t.coeff * term.coeff))
    scaled
  }

  def multiplyPolys(poly1: Polynomial, poly2: Polynomial): Polynomial = {
    val preRes = for {
      Term(c1, p1) <- poly1
      Term(c2, p2) <- poly2
    } yield Term(c1 * c2, p1 + p2)
    collapseDups(skipZeroTerms(preRes))
  }

  def raisePolyToPower(poly: Polynomial, pow: Int): Polynomial = {
    require(pow >= 0, "Can't reaise polynomial to negative power")
    if (pow == 0) s(Term.zero(poly.head.powers.dim))
    else if (pow == 1) poly
    else {
      // TODO: Use Newton Binomial
      Seq.fill(pow)(poly) reduce multiplyPolys
    }
  }

  def collapseDups(poly: Polynomial): Polynomial = {
    val res = poly groupBy (_.powers) map (group => group._2) map (_.reduceLeft((t1, t2) => t1 withCoeff (t1.coeff + t2.coeff)))
    skipZeroTerms(res.toIndexedSeq)
  }

  def skipZeroTerms(poly: Polynomial): Polynomial =
    poly filterNot (_.coeff.isZero)

}