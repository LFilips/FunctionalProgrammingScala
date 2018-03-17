package chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamTest extends FlatSpec with Matchers {

  "the toList method" should "convert a stream in a List" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.toList should be(List(1, 2, 3, 4, 5))

  }

  it should "give an Nil when invoked on an Empty stream" in {

    val stream = Empty

    stream.toList should be(Nil)

  }

  "the drop method" should " drop the first n element" in {

    val stream = Stream(1, 2, 3, 4, 5)

    //stream.drop(3) should be(Cons(() => 4, () => Stream(5)))

    stream.drop(3).toList should be(List(4, 5))

  }

  "the drop method" should "give empty is invoked on a empty stream" in {

    val stream = Empty

    stream.drop(3) should be(Empty)

  }

  "take" should "return take the first n element" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.take(2).toList should be(List(1, 2))

  }

  "take" should "return empty when invoked on empty list" in {

    val stream = Empty

    stream.take(2) should be(Empty)

  }

  "take" should "behave correctly with infinite stream" in {

    val stream = Stream.constant(1)

    stream.take(2).toList should be(List(1, 1))

  }


  "take2" should "return take the first n element" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.take2(2).toList should be(List(1, 2))

  }

  "take2" should "return empty when invoked on empty list" in {

    val stream = Empty

    stream.take2(2) should be(Empty)

  }

  "takewhile" should "return elements that are matching the predicate" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.takeWhile((x) => (x <= 2)).toList should be(List(1, 2))

  }

  "takewhile" should "return empty when used on empty stream" in {

    val stream = Empty

    stream.takeWhile((x) => true) should be(Empty)

  }

  "takewhile" should "return elements that are matching the predicate on infinite stream" in {

    val stream = Stream.fibsUnfolded()

    stream.takeWhile((x) => (x <= 150)).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))

  }


  "takewhileFolded" should "return elements that are matching the predicate" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.takeWhileFolded((x) => (x <= 2)).toList should be(List(1, 2))

  }

  "takewhileFolded" should "return elements that are matching the predicate with String" in {

    val stream = Stream("hi", "hello", "ham", "hamburger", "Not valid")

    stream.takeWhileFolded((x) => x.startsWith("h")).toList should be(List("hi", "hello", "ham", "hamburger"))

  }

  "takewhileFolded" should "return empty Stream when there is no matching" in {

    val stream = Stream("hi", "hello", "ham", "hamburger", "Not valid")

    stream.takeWhileFolded((x) => x.startsWith("k")) should be(Empty)

  }

  "takewhileFolded" should "work correctly with infinite stream" in {

    val stream = Stream.fibsUnfolded()

    stream.takeWhileFolded((x) => x < 150).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))

  }

  "forAll" should "return true if all elements match the predicate" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.forAll((x) => x.isInstanceOf[Int]) should be(true)

  }

  "forAll" should "return false if one element don't match the predicate" in {

    val stream = Stream(1, 2, "hi", 4, 5)

    stream.forAll((x) => x.isInstanceOf[Int]) should be(false)

  }


  "foldRight" should "recursively apply the funtion to all the element of the list" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.foldRight(0)(_ + _) should be(15)

  }

  "headOption" should "give an Option with the head of the Stream if the head exist" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.headOption() should be(Some(1))

  }

  "headOption" should "give a None when the Stream is empty" in {

    val stream = Empty

    stream.headOption() should be(None)

  }


  "headOptionFoldRight" should "give an Option with the head of the Stream if the head exist" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.headOptionFoldRight() should be(Some(1))

  }

  "headOptionFoldRight" should "give a None when the Stream is empty" in {

    val stream = Empty

    stream.headOptionFoldRight() should be(None)

  }

  "map" should "apply the function to all the element of the stream" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.map(_ * 2).toList should be(List(2, 4, 6, 8, 10))

  }

  "map" should "return empty if applied on empty stream" in {

    val stream = Empty

    stream.map(_.toString) should be(Empty)

  }

  "the map method" should "work on infinity stream" in {

    lazy val infiniteStream: Stream[Int] = Stream.cons(1, infiniteStream) //this shoud be defined as lazy, otherwise will have forward reference error

    infiniteStream.map(_ * 2).take(5).toList should be(List(2, 2, 2, 2, 2))


    /**
      *
      * Important note:
      *
      * This was not working (stackoverflow), so the stream was evaluated instead of being lazy) because I've forget to put the lazy definition in the foldRight signature
      *
      * def foldRight[B](z: => B)(f: (A, B) B) : B =               instead of
      * def foldRight[B](z: => B)(f: (A, => B) => B) : B =
      *
      * This is my fold definition:
      * case Cons(head,tail) => f(head(),tail().foldRight(z)(f))
      *
      * There were 2 lazy missing in z and in the B in f. The lack in z didn't create many problem, but the empty Stream was evaluated before then necessary, but the big
      * deal was the function beacuse without the lazy evaluate the tail of the stream is evaluated and in case of an infinite Stream will lead to a stack overflow.
      *
      */

  }

  "map" should "not go in overflow when used on a infinite fibonacci stream" in {

    val stream = Stream.fibsUnfolded()

    stream.map(_ * 2).take(6).toList should be(List(0, 2, 2, 4, 6, 10))

  }

  "append" should "append the input Stream" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.append(Stream(1, 2, 3)).toList should be(List(1, 2, 3, 4, 5, 1, 2, 3))

  }

  "flatmap" should "apply the function to each element and flatten the result" in {

    val stream = Stream(1, 2, 3, 4)

    stream.flatMap((x) => Stream(x, x)).toList should be(List(1, 1, 2, 2, 3, 3, 4, 4))

  }


  "the take method" should "work on infinity stream" in {

    lazy val infiniteStream: Stream[Int] = Stream.cons(1, infiniteStream) //this shoud be defined as lazy, otherwise will have forward reference error

    infiniteStream.take(5).toList should be(List(1, 1, 1, 1, 1))
  }


  "The constant method" should "create an infinite stream with the value provided" in {

    val infiniteStream = Stream.constant[Int](1)

    infiniteStream.map(_ * 2).take(5).toList should be(List(2, 2, 2, 2, 2))

  }


  "The from method" should "create an infinite stream increasing int" in {

    val infiniteStream = Stream.from(2)

    infiniteStream.take(5).toList should be(List(2, 3, 4, 5, 6))

  }


  "Fibs" should "create an infinite stream with fibonacci sequence" in {

    val infiniteStream = Stream.fibs()

    infiniteStream.take(13).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))

  }


  "Unfold" should "create an infinite stream using the generation function" in {

    val infiniteStream = Stream.unfold(0)((s) => if (s != 5) Some(1, s + 1) else None) //the state is represented by an integer, when the state is 5 i have a none and the stream should terminate

    infiniteStream.take(7).toList should be(List(1, 1, 1, 1, 1))

  }

  "unfolded ones" should "correctly generate an infinite stream" in {

    Stream.onesUnfolded().take(5).toList should be(List(1, 1, 1, 1, 1))

  }

  "constantUnfolded" should "create an infinite stream with the provided constant" in {

    Stream.constantUnfolded(3).take(5).toList should be(List(3, 3, 3, 3, 3))

  }

  "unfolded fibs" should "correctly generate an infinite stream" in {

    Stream.fibsUnfolded().take(13).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))

  }

  "mapUnfold" should "apply the function to all the element of the stream" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.mapUnfold(_ * 2).toList should be(List(2, 4, 6, 8, 10))

  }

  "mapUnfold" should "return empty if applied on empty stream" in {

    val stream = Empty

    stream.mapUnfold(_.toString) should be(Empty)

  }

  "mapUnfold" should "not go in overflow when used on a infinite stream" in {

    val stream = Stream.fibsUnfolded()

    stream.mapUnfold(_ * 2).take(6).toList should be(List(0, 2, 2, 4, 6, 10))

  }

  "takeUnfold" should "return take the first n element" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.takeUnfold(2).toList should be(List(1, 2))

  }

  "takeUnfold" should "return empty when invoked on empty list" in {

    val stream = Empty

    stream.takeUnfold(2) should be(Empty)

  }

  "takeUnfold" should "behave correctly with infinite stream" in {

    val stream = Stream.constant(1)

    stream.takeUnfold(2).toList should be(List(1, 1))

  }

  "takeWhileUnfold" should "return take the first n element" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.takeWhileUnfold(_<3).toList should be(List(1, 2))

  }

  "takeWhileUnfold" should "return empty when invoked on empty list" in {

    val stream = Empty

    stream.takeWhileFolded((x) => true) should be(Empty)

  }

  "takeWhileUnfold" should "behave correctly with infinite stream" in {

    val stream = Stream.fibsUnfolded()

    stream.takeWhileFolded(_<150).toList should be(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144))

  }

  "zipWithFunctionUnfold" should "zip two list according the provided function" in {

    val streamA = Stream(1, 2, 3, 4, 5)
    val streamB = Stream(1, 1, 1, 1, 1)

    streamA.zipWithFunctionUnfold(streamB)((a, b) => a + b).toList should be(List(2, 3, 4, 5, 6))

  }

  "zipWithFunctionUnfold" should "zip two list of different size, until the shorter has value" in {

    val streamA = Stream(1, 2, 3, 4, 5)
    val streamB = Stream(1, 1, 1)

    streamA.zipWithFunctionUnfold(streamB)((a, b) => a + b).toList should be(List(2, 3, 4))

  }

  "zipWithFunctionUnfold" should "zip two list of different size, until the shorter has value (2)" in {

    val streamA = Stream(1, 1, 1)
    val streamB = Stream(1, 2, 3, 4, 5)

    streamA.zipWithFunctionUnfold(streamB)((a, b) => a + b).toList should be(List(2, 3, 4))

  }

  "zipWithFunctionUnfold" should "return an empty stream if invoked on empty streams" in {

    val streamA = Empty
    val streamB = Empty

    streamA.zipWithFunctionUnfold(streamB)((a, b) => Cons(a,b)) should be(Empty)

  }


  "ZipWithUnfold" should "zip two stream in a stream that contains tuple with value from each of them" in {

    val streamA = Stream(1, 2, 3, 4)
    val streamB = Stream("one", "two", "three", "four")

    streamA.zipWithUnfold(streamB).toList should be(List((1,"one"),(2,"two"),(3,"three"),(4,"four")))

  }

  "ZipWithUnfold" should "zip two list of different size, until the shorter has value" in {

    val streamA = Stream(1, 2, 3, 4, 5)
    val streamB = Stream("one", "two")

    streamA.zipWithUnfold(streamB).toList should be(List((1,"one"),(2,"two")))

  }

  "ZipWithUnfold" should "zip two list of different size, until the shorter has value (2)" in {

    val streamA = Stream(1, 2)
    val streamB = Stream("one", "two", "three", "four")

    streamA.zipWithUnfold(streamB).toList should be(List((1,"one"),(2,"two")))

  }

  "ZipWithUnfold" should "return an empty stream if invoked on empty streams" in {

    val streamA = Empty
    val streamB = Empty

    streamA.zipWithUnfold(streamB) should be(Empty)

  }

  //    streamA.zipWithUnfold(streamB).toList should be(List((Some(1),Some("one")),(Some(2),Some("two")),(Some(3),Some("three")),(Some(4),Some("four"))))


  "zipAll" should "zip two stream in a stream on tuple with Option" in {

    val streamA = Stream(1, 2, 3, 4)
    val streamB = Stream("one", "two", "three", "four")

    streamA.zipAll(streamB).toList should be(List((Some(1),Some("one")),(Some(2),Some("two")),(Some(3),Some("three")),(Some(4),Some("four"))))

  }

  "zipAll" should "zip two list of different size, until the shorter has value" in {

    val streamA = Stream(1, 2, 3, 4)
    val streamB = Stream("one", "two")

    streamA.zipAll(streamB).toList should be(List((Some(1),Some("one")),(Some(2),Some("two")),(Some(3),None),(Some(4),None)))

  }

  "zipAll" should "zip two list of different size, until the shorter has value (2)" in {

    val streamA = Stream(1, 2)
    val streamB = Stream("one", "two", "three", "four")

    streamA.zipAll(streamB).toList should be(List((Some(1),Some("one")),(Some(2),Some("two")),(None,Some("three")),(None,Some("four"))))

  }

  "zipAll" should "return an empty stream if invoked on empty streams" in {

    val streamA = Empty
    val streamB = Empty

    streamA.zipAll(streamB) should be(Empty)

  }


}
