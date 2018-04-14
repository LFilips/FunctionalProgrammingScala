package chapter10

import chapter10.Monoid.{stringMonoid, listMonoid}
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  trait StringFixture {
    val (first, second, third) = ("a", "b", "c")
  }

  trait ListFixture {
    val (first, second, third) = (List(1), List(2), List(3))
  }

  "stringMonoid" should "combine two string with the op operator" in new StringFixture {

    stringMonoid.op(first, second) should be(s"$first$second")
  }

  it should "respect the associativity law for op" in new StringFixture {

    //(a + b) + c = a + (b + c)
    val leftSide = stringMonoid.op(stringMonoid.op(first, second), third)
    val rightSide = stringMonoid.op(first, stringMonoid.op(second, third))
    leftSide should be(rightSide)

  }

  it should "leave the element as the same when combined with the zero element" in new StringFixture {

    val elem = "a"
    stringMonoid.op(elem, stringMonoid.zero) should be(elem)

  }

  /**
    * I should thing a bit more about how test the monad instance:
    *
    * 1) I have to test the operation per se?
    * 2) For sure i need to test the associativity rule
    * 3) I need to the test the validity of the zero element
    */



}
