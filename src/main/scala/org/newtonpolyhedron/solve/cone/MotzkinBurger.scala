package org.newtonpolyhedron.solve.cone

import org.newtonpolyhedron.NewtonImports._

/**
 * Motzkin-Burger implementation of cone solver.
 *
 * Runtime: O(n^4^ d) worst-case, n - #inequations, d - dimension
 */
class MotzkinBurger extends ConeSolver {

  /** Linear Inequations */
  private type L = Seq[IntVec]

  /** Basis Vectors */
  private type U = Seq[IntVec]

  /** Fundamental Solutions */
  private type V = Seq[IntVec]

  override def solve(
      ineqs:       L,
      basisOption: Option[U],
      dimension:   Int
  ): V = {
    require(ineqs forall (_.size == dimension), "Inequation vector with incorrect dimension")
    // E.g. we got one vector in 3d space - we can handle 2 vectors here (simple degenerated case),
    // but with this few points the solution is undefined
    require(ineqs.size >= dimension - 1, s"Not enough equations given, need at least ${dimension - 1}."
      + " This case is too degenerated to have any solutions.")
    val basis: U = basisOption getOrElse (
      // Default basis
      (0 until dimension) map (IntVec.zero(dimension).upd(_, 1))
    )
    require(basis forall (_.size == dimension), "Basis vector with incorrect dimension")
    solve(ineqs, basis, dimension)
  }

  private def solve(
      ineqs:     L,
      basis:     U,
      dimension: Int
  ): V = {
    val lInitial: Seq[IntVec] = Seq(IndexedSeq.fill(dimension)(0))
    val (us, vs) = recurse(lInitial, ineqs)(basis, Seq.empty)
    if (us.isEmpty) {
      vs
    } else {
      assert(us.size == 1, "Unexpected basis left in the end: " + us)
      // If after all we still have single remaining basis vector, then
      // this is simple degenerate case of (N - 1)-dimensional cone in N-dimensional space,
      // e.g. 2-vectors plane in 3d space.
      // In this case, both remaining vector and its negation are conforming.
      (us ++ us.map(vec => -vec)).distinct
    }
  }

  /**
   * Solve inequations recursively via Motzkin-Burger algorithm
   *
   * @param ls all linear inequations in system
   * @param lRem linear inequations which aren't recursed over yet
   * @param us basis of current step
   * @param vs solutions of current step
   */
  private def recurse(ls: L, lRem: L)(us: U, vs: V): (U, V) = lRem match {
    case nil if nil.isEmpty =>
      (us, vs)
    case l +: lRest if us exists (_ *+ l != 0) =>
      val (us2, vs2) = solveWithBasisVec(l)(us, vs)
      recurse(ls :+ l, lRest)(us2, vs2)
    case l +: lRest =>
      val vs2 = solveWithoutBasisVec(l, ls)(vs)
      recurse(ls :+ l, lRest)(us, vs2)
  }

  /**
   * Motzkin-Burger step - general case
   *
   * @param l linear inequation currently being recursed over
   * @param us basis of current step
   * @param vs solutions of current step
   */
  private def solveWithBasisVec(l: IntVec)(us: U, vs: V): (U, V) = {
    // Inversing the signs of positive-conforming basis vectors
    val bs = us map (u => if (u *+ l <= 0) u else -u)
    val Some(bSel) = bs find (_ *+ l != 0)
    val lbSel = l *+ bSel
    val us2 =
      for {
        b <- bs if b != bSel
        lb = l *+ b
      } yield ((b * lbSel) - (bSel * lb)).reduced
    val vs2 = {
      for {
        v <- vs
        lv = l *+ v
      } yield (-(v * lbSel) + (bSel * lv)).reduced
    }
    (us2.distinct, (bSel +: vs2).distinct)
  }

  /**
   * Motzkin-Burger step - degenerate case
   *
   * @param l linear inequation currently being recursed over
   * @param ls all linear inequations in system
   * @param vs solutions of current step
   */
  private def solveWithoutBasisVec(l: IntVec, ls: L)(vs: V): V = {
    val `vs-` = vs filter (_ *+ l < 0)
    val `vs0` = vs filter (_ *+ l == 0)
    val `vs+` = vs filter (_ *+ l > 0)
    val `vs±` = for {
      `v-` <- `vs-`
      `v+` <- `vs+`
      // TODO: always combine in 2d space?
      if /* `v-`.size == 2 ||*/ vs.size == 2 || shouldCombine(ls, vs)(`v-`, `v+`)
    } yield {
      val `lv-` = `v-` *+ l
      val `lv+` = `v+` *+ l
      val `v±` = ((`v-` * `lv+`) - (`v+` * `lv-`)).reduced
      if (ls forall (_ *+ `v±` <= 0)) {
        `v±`
      } else {
        -`v±`
      }
    }
    (`vs-` ++ `vs0` ++ `vs±`).distinct
  }

  /**
   * Whether or not the given pair of solutions should be combined in Motzkin-Burger degenerate case step
   *
   * @param ls all linear inequations in system
   * @param vs solutions of current step
   */
  private def shouldCombine(ls: L, vs: V)(v1: IntVec, v2: IntVec): Boolean = {
    // Inequations yielding zeros for both vectors
    val `ls*` = ls filter (l => (l *+ v1 == 0) && (l *+ v2 == 0))
    // Vectors excluding given pair
    val `vs*` = vs filter (v => (v != v1) && (v != v2))
    `vs*` forall (v =>
      `ls*` exists (l =>
        l *+ v != 0))
  }
}
