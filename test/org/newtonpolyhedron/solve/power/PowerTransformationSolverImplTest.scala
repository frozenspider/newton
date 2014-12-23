package org.newtonpolyhedron.solve.power

import scala.collection.immutable.SortedSet

import org.fs.utils.collection.table.ArrayListKeyTable
import org.fs.utils.collection.table.KeyTable
import org.fs.utils.collection.table.KeyTables
import org.junit.runner.RunWith
import org.newtonpolyhedron.entity.vector.VectorImports._
import org.newtonpolyhedron.solve.eqsys.SimpleEqSystemSolverImpl
import org.newtonpolyhedron.solve.matrixuni.UnimodularMatrixMakerImpl
import org.newtonpolyhedron.test._
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PowerTransformationSolverImplTest extends FunSuite {

  val solver = new PowerTransformationSolverImpl(new UnimodularMatrixMakerImpl, new SimpleEqSystemSolverImpl)

  test("choose pairs") {
    val source = s(
      s(1, 2, 3),
      s(0.1, 0.2),
      s("a", "b", "c"))
    val expected = s(
      s((1, 2), (0.1, 0.2), ("a", "b")),
      s((1, 2), (0.1, 0.2), ("b", "c")),
      s((2, 3), (0.1, 0.2), ("a", "b")),
      s((2, 3), (0.1, 0.2), ("b", "c"))
    )

    val actual = solver.choosePairs(source)
    assert(actual.toList === expected)
  }

  /*
 3
+40, 1 1 1
+25, 4 0 0
-25, 0 4 0
-1,  0 0 4
+16, 2 0 2
%
-1, 0 1 1
-1, 2 0 1
-1, 3 1 0
-1, 2 1 1
#
0 1 3 4
0 1
@


-3 1 1
2 -1 0
-1 1 0

solution/-1 -1

z1 = -14/3 y2^3 + O
z2 = -2 y3^2 + O

solution/5 -1

z1 = -68/15 y2^3 + O
z2 = -4/5 y3^2 + O
*/
}
