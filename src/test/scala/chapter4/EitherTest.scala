package chapter4

import org.scalatest.{FlatSpec, Matchers}

class EitherTest extends FlatSpec with Matchers {

  "The map method" should "map the element if a right" in {

    Right(3).map((x) => (x * 2)) should be(Right(6))

  }

  it should "give a Left is used on a left (without execution the function)" in {

    val either: Either[String, Int] = Left("Error")

    either.map((x) => (x * 3)) should be(Left("Error"))
  }

  it should "give a Left is used on a left (without execution the function) when the left is an error" in {

    val exception = new RuntimeException("There was an exception")

    val either: Either[Exception, Int] = Left(exception)

    either.map((x) => (x * 3)) should be(Left(exception))
  }

  "The orElse method" should "give the value if a Right, otherwise the value provided" in {

    val either : Either[String,Int] = Right(3)

    either.orElse(Right(6)) should be(Right(3))
  }

  it should "give the right provided when left" in {

    val either : Either[String,Int] = Left("hello")

    either.orElse(Right(6)) should be(Right(6))
  }

  it should "give the left provided when left" in {

    val either : Either[String,Int] = Left("hello")

    Left("Hello").orElse(Left(6)) should be(Left(6))

  }

  "The flatMap method" should "apply the function when called on a right" in {

    val either : Either[String,Int] = Right(3)

    either.flatMap((x) => Right(x*2)) should be(Right(6))

  }

  it should "be capable of producing a left either with the function provided" in {

    val either : Either[String,Int] = Right(3)

    either.flatMap((x) => Left("Error")) should be(Left("Error"))

  }

  it should "not call the function when invoked on a either left" in {

    val either : Either[String,Int] = Left("Error")

    either.flatMap((x) => Right(6)) should be(Left("Error"))

  }

}
