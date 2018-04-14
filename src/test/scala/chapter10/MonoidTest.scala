package chapter10

import chapter10.Monoid.{stringMonoid, listMonoid}
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  trait MonoidFixture[A] {
    val tuple: (A, A, A)
    val first : A
    val second : A
    val third : A
  }

  trait StringFixture extends MonoidFixture[String] {
    val tuple = ("a", "b", "c")
    val first = tuple._1
    val second = tuple._1
    val third = tuple._1
  }

  trait ListFixture extends MonoidFixture[List[Int]] {
    val tuple = (List(1), List(2), List(3))
    val first = tuple._1
    val second = tuple._1
    val third = tuple._1
  }

  /**
    * Is a list of tuple, where I provide the instance of the monoid to test,
    * 3 element of the type of the monoid
    * the operation that the monoid perform
    */

  /** FIXME make it more generic
    * val monoidList = List(
    * (stringMonoid, new StringFixture {}, (a: String, b: String) => a + b),
    * (listMonoid[Int] _, new ListFixture {}, (a: List[Int], b: List[Int]) => a ++ b)
    * )
    */


  "StringMonoid" should "have op, zero and respect associativity" in {

    val tuple = (stringMonoid, new StringFixture {}, (a: String, b: String) => a + b)

    val monoidUnderTest = tuple._1
    val monoidFixture = tuple._2
    val operator = tuple._3

    //testing the operation
    monoidUnderTest.op(monoidFixture.first, monoidFixture.second) should be(operator(monoidFixture.first, monoidFixture.second))

    //testing associativity rules
    val leftSide = monoidUnderTest.op(monoidUnderTest.op(monoidFixture.first, monoidFixture.second), monoidFixture.third)
    val rightSide = monoidUnderTest.op(monoidFixture.first, monoidUnderTest.op(monoidFixture.second, monoidFixture.third))
    leftSide should be(rightSide)

    //testing zero value combination
    monoidUnderTest.op(monoidFixture.first, monoidUnderTest.zero) should be(monoidFixture.first)
    monoidUnderTest.op(monoidUnderTest.zero, monoidFixture.first) should be(monoidFixture.first)
  }

  "ListMonoid" should "have op, zero and respect associativity" in {

    val listIntMonoid = listMonoid[Int]

    val tuple = (listIntMonoid, new ListFixture {}, (a: List[Int], b: List[Int]) => a ++ b)

    val monoidUnderTest = tuple._1
    val monoidFixture = tuple._2
    val operator = tuple._3

    //testing the operation
    monoidUnderTest.op(monoidFixture.first, monoidFixture.second) should be(operator(monoidFixture.first, monoidFixture.second))

    //testing associativity rules
    val leftSide = monoidUnderTest.op(monoidUnderTest.op(monoidFixture.first, monoidFixture.second), monoidFixture.third)
    val rightSide = monoidUnderTest.op(monoidFixture.first, monoidUnderTest.op(monoidFixture.second, monoidFixture.third))
    leftSide should be(rightSide)

    //testing zero value combination
    monoidUnderTest.op(monoidFixture.first, monoidUnderTest.zero) should be(monoidFixture.first)
    monoidUnderTest.op(monoidUnderTest.zero, monoidFixture.first) should be(monoidFixture.first)
  }


}