package org.newtonpolyhedron.utils.parsing

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.newtonpolyhedron.entity.BigFrac
import java.util.Scanner
import org.apache.commons.math3.exception.MathParseException
import java.text.ParseException

@RunWith(classOf[JUnitRunner])
class BigFracFormatTest extends FunSuite {

  val fmt = BigFracFormat

  test("int") {
    assert(fmt.parse("123") === BigFrac(123))
  }

  test("fraction") {
    assert(fmt.parse("123/456") === BigFrac(123, 456))
  }

  test("decimal") {
    assert(fmt.parse("123.456") === BigFrac(123456, 1000))
  }

  test("decimal, no leading zero") {
    assert(fmt.parse(".456") === BigFrac(456, 1000))
  }

  test("multiple") {
    val str = "   " +
      "123" + " \n " +
      "321" + "\t" +
      "999/2" + "      " +
      "-123.46" + " " +
      "asd"
    val scanner = new Scanner(str)
    assert(fmt.parse(scanner.next) === BigFrac(123))
    assert(fmt.parse(scanner.next) === BigFrac(321))
    assert(fmt.parse(scanner.next) === BigFrac(999, 2))
    assert(fmt.parse(scanner.next) === BigFrac(-12346, 100))
  }

  test("non-digital string") {
    intercept[ParseException] { fmt.parse("qwe") }
  }

  test("wrong separator") {
    intercept[ParseException] { fmt.parse(",123") }
  }
}