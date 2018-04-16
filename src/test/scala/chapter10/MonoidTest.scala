package chapter10

import chapter10.Monoid.{stringMonoid, listMonoid}
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  trait MonoidFixture[A] {
    val tuple: (A, A, A)
    val first: A
    val second: A
    val third: A
  }

  trait StringFixture extends MonoidFixture[String] {
    val tuple = ("a", "b", "c")
    val first = tuple._1
    val second = tuple._1
    val third = tuple._1
  }

  trait intFixture extends MonoidFixture[Int] {
    val tuple = (1,2,3)
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
    * Generic function that tests the rules of a Monoid.
    * @tparam A the monoid type
    */
  def testMonoidRules[A](tuple: (Monoid[A],MonoidFixture[A],(A, A) => A)) :Unit = {
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


  private val MonoidRules = "have op, zero and respect associativity"

  "StringMonoid" should MonoidRules in {

    val tuple = (stringMonoid, new StringFixture {}, (a: String, b: String) => a + b)

    testMonoidRules[String](tuple)
  }

  "ListMonoid" should MonoidRules in {

    val listIntMonoid = listMonoid[Int]

    val tuple = (listIntMonoid, new ListFixture {}, (a: List[Int], b: List[Int]) => a ++ b)

    testMonoidRules[List[Int]](tuple)

  }


  "intAdditionshould" should MonoidRules in {

    val tuple = (Monoid.intAddition, new intFixture {}, (a:Int, b: Int) => a + b)

    testMonoidRules[Int](tuple)

  }


  /*"endoMonoid" should MonoidRules in {

    val endoMonoid = endoMonoid[Int] _

    val tuple = (endoMonoid, new ListFixture {}, (a: List[Int], b: List[Int]) => a ++ b)

    testMonoidRules[List[Int]](tuple)


  }*/


}