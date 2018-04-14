package chapter10

import chapter10.Monoid.{stringMonoid,listMonoid}
import org.scalatest.{FlatSpec, Matchers}

class MonoidTest extends FlatSpec with Matchers {

  "stringMonoid" should "combine two elememt" in {

    stringMonoid.op("Hello ","There") should be("Hello There")

  }

  "stringMonoid" should "leave the element as the same when combined with the zero element" in {

    stringMonoid.op("Hello",stringMonoid.zero) should be("Hello")

  }

  "listMonoid" should "combine two string" in {

    listMonoid.op(List("Hello"),List("There")) should be(List("Hello","There"))

  }

  "listMonoid" should "leave the element as the same when combined with the zero element" in {

    listMonoid.op(List("Hello"),listMonoid.zero) should be(List("Hello"))

  }

}
