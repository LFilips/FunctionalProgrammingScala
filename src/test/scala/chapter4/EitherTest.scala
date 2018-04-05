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

  "the sequence method" should "create an Either cointainig a List of all the either value, if all are right" in {

    val eitherList : List[Either[String,Int]] = List(Right(1),Right(2),Right(3),Right(4))

    Either.sequence(eitherList) should be(Right(List(1,2,3,4)))

  }

  it should "create an Either cointaining the first error encountered in the list" in {

    val eitherList : List[Either[String,Int]] = List(Right(1),Left("error"),Right(3),Right(4))

    Either.sequence(eitherList) should be(Left("error"))

  }

  "the traverse method" should "traverse the list of Either applyng the fucntion that can fail, and if succeed give a list of result" in {

    val eitherList : List[String] = List("1","2","3","4")

    Either.traverse(eitherList)((a) => Either.Try(a.toInt) ) should be(Right(List(1,2,3,4)))

  }

  it should "give the first error encountered in the function than can create an either" in {

    val eitherList : List[String] = List("1","Hello","3","4")

    Either.traverse(eitherList)((a) => Either.Try(a.toInt) ) shouldBe a [Left[_]]


  }

}
