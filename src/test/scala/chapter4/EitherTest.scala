package chapter4

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  "The map method" should "map the element if a right" in {

    Right(3).map((x) => (x*2)) should be(Right(6))

  }

}
