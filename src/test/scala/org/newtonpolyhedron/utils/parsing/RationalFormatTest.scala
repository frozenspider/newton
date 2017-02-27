package org.newtonpolyhedron.utils.parsing

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import java.util.Scanner
import org.apache.commons.math3.exception.MathParseException
import java.text.ParseException
import spire.math.Rational

@RunWith(classOf[JUnitRunner])
class RationalFormatTest extends FunSuite {

  val fmt = RationalFormat

  test("int") {
    assert(fmt.parse("123") === Rational(123))
  }

  test("fraction") {
    assert(fmt.parse("123/456") === Rational(123, 456))
  }

  test("decimal") {
    assert(fmt.parse("123.456") === Rational(123456, 1000))
  }

  test("decimal, no leading zero") {
    assert(fmt.parse(".456") === Rational(456, 1000))
  }

  test("multiple") {
    val str = "   " +
      "123" + " \n " +
      "321" + "\t" +
      "999/2" + "      " +
      "-123.46" + " " +
      "asd"
    val scanner = new Scanner(str)
    assert(fmt.parse(scanner.next) === Rational(123))
    assert(fmt.parse(scanner.next) === Rational(321))
    assert(fmt.parse(scanner.next) === Rational(999, 2))
    assert(fmt.parse(scanner.next) === Rational(-12346, 100))
  }

  test("non-digital string") {
    intercept[ParseException] { fmt.parse("qwe") }
  }

  test("wrong separator") {
    intercept[ParseException] { fmt.parse(",123") }
  }
}
