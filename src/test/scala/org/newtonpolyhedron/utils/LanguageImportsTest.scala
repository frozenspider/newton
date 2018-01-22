package org.newtonpolyhedron.utils

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.InputParser

@RunWith(classOf[JUnitRunner])
class LanguageImportsTest extends FunSuite {
  import LanguageImports._

  def toLines(s: String) = InputParser.refineLines(s.lines.toStream)

  //
  // Parse vectors
  //

  test("n choose k") {
    assert((0 choose 0) === 1)
    assert((1 choose 0) === 1)
    assert((2 choose 0) === 1)
    assert((2 choose 2) === 1)
    assert((2 choose 1) === 2)
    assert((5 choose 3) === 10)
  }
}
