package chapter4

import org.scalatest.{FlatSpec, Matchers}

class OptionTest extends FlatSpec with Matchers {

  "the average method" should "return a Some with the average" in {

    val list = List(1.0, 2.0, 3.0)

    Chapter4.mean(list) should be(Some(2.0))

  }

  "the average method" should "return a None for an empty list" in {

    val list = List()

    Chapter4.mean(list) should be(None)

  }

  "The map method" should "map the option with the function provided" in {

    val option = Some(3)

    val result = option.map((x) => x * 3)

    result should be(Some(9))

  }

  "The map method" should "produce a none when used on none" in {

    val option: Option[Int] = None

    val result = option.map((x) => x * 4)

    result should be(None)

  }

  "The map method" should "be able to chain many operation" in {

    val option: Option[Int] = Some(3)

    val result = option.map(_ * 4).map(_ / 2)

    result should be(Some(6))

  }

  "The flatMap method" should "be able to produce a Some" in {

    val option = Some(3)

    val result = option.flatMap((x) => Some(x))

    result should be(Some(3))

  }

  "The flatMap method" should "be able give produce a None" in {

    val option = Some(List())

    val result = option.flatMap((x) => (if (x.isEmpty) None else Some(x.head)))

    result should be(None)

  }

  "The getOrElse method" should "return the contained value in case of Some" in {

    val option = Some(3)

    val result = option.getOrElse(5)

    result should be(3)

  }

  "The getOrElse method" should "return the default value in case of Nome" in {

    val option = None

    val result = option.getOrElse(5)

    result should be(5)

  }

  "The filter method" should "not filter out element that doesn't match the predicate" in {

    val option = Some(4)

    val result = option.filter(_ % 2 == 1)

    result should be(None)

  }

  "The filter method" should "filter out element that doesn't match the predicate" in {

    val option = Some(4)

    val result = option.filter(_ % 2 == 0)

    result should be(Some(4))

  }

  "The variance method" should "correctly calculate the variance" in {

    val list = List(1.0, 2.0, 3.0)

    Chapter4.variance(list) should be(Some(0.6666666666666666))

  }

  "The Try method" should "return a none when an exception is thrown" in {

    lazy val functionWithException = {
      throw new RuntimeException()
    } //need to be lazy to have the exception in the try method

    Chapter4.Try(functionWithException) should be(None)

  }

  "The Try method" should "return a none when an exception is thrown without using lazy keyword" in {

    Chapter4.Try("This will throw an exception".toInt) should be(None)

  }

  "The Try method" should "return a Some when no exception thrown" in {

    Chapter4.Try("2".toInt) should be(Some(2))

  }

  "The map2 method" should "combine two option values witha binary operation" in {

    Chapter4.map2(Some(3), Some(4))((x, y) => x + y) should be(Some(7))

  }

  "The map2 method" should "get a None when one is None" in {

    Chapter4.map2(None, Some(4))((x: Int, y: Int) => x + y) should be(None)

  }

  "The sequence method" should "transform a List[Option[A]] into an Option[List[A]] is no None is present" in {

    val list: List[Option[Int]] = List(Some(3), Some(6), Some(7), Some(8)) //forcing the type to option

    Option.sequenceUsingFlatMap(list) should be(Some(List(3, 6, 7, 8)))

  }

  "The sequence method" should " give an None when there is a none in the list" in {

    val list: List[Option[Int]] = List(Some(3), Some(6), None, Some(8)) //forcing the type to option

    Option.sequenceUsingFlatMap(list) should be(None)

  }


}
