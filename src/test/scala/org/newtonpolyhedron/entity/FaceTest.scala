package org.newtonpolyhedron.entity

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FaceTest extends FunSuite {
  test("ordering") {
    val faces = Seq(
      new Face(Seq(0, 1, 2)),
      new Face(Seq(0, 1, 3, 4)),
      new Face(Seq(0, 2, 3)),
      new Face(Seq(1, 2, 3, 4))
    )
    faces.permutations.toList map { perm =>
      assert(perm.sorted === faces)
    }
  }
}
