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

  "map" should "apply the function to all the elemnt of the stream" in {

    val stream = Stream(1, 2, 3, 4, 5)

    stream.map(_ * 2).toList should be(List(2, 4, 6, 8, 10))

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

  "the map method" should "work on infinity stream" in {

    lazy val infiniteStream: Stream[Int] = Stream.cons(1, infiniteStream) //this shoud be defined as lazy, otherwise will have forward reference error

    infiniteStream.map(_ * 2).take(5).toList should be(List(2, 2, 2, 2, 2))


    /**
      *
      * Important note:
      *
      * This was not working (stackoverflow, so the stream was evaluated instead of being lazy) because I've forget to put the lazy definition in the foldRight signature
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

    infiniteStream.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))

  }


  "Unfold" should "create an infinite stream using the generation function" in {

    val infiniteStream = Stream.unfold(0)((s) => if (s != 5) Some(1, s + 1) else None) //the state is represented by an integer, when the state is 5 i have a none and the stream should terminate

    infiniteStream.take(7).toList should be(List(1, 1, 1, 1, 1))

  }

}
