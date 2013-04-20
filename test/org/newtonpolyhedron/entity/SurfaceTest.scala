package org.newtonpolyhedron.entity

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SurfaceTest extends FunSuite {
  test("ordering") {
    val surfaces = Seq(
      new Surface(Seq(0, 1, 2)),
      new Surface(Seq(0, 1, 3, 4)),
      new Surface(Seq(0, 2, 3)),
      new Surface(Seq(1, 2, 3, 4)))
    surfaces.permutations.toList map { perm =>
      assert(perm.sorted === surfaces)
    }
  }
}