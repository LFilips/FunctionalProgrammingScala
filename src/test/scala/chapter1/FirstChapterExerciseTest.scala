package chapter1

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks._
import chapter1.FirstChapterExercise._


class FirstChapterExerciseTest extends FlatSpec with Matchers {


  val fibonacciInput =
    Table(
      ("index", "result"),
      (0, 0),
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 5),
      (6, 8),
      (23, 28657),
      (29, 514229),
      (32, 2178309)
    )

  val sortedInput =
    Table(
      ("array", "result"),
      (Array(1, 2, 3, 4, 5, 6), true),
      (Array(), true),
      (Array(1, 2), true),
      (Array(2, 1), false),
      (Array(1), true),
      (Array(2, 4, 2, 5, 1), false)
    )


  "The fibonacci method" should "return the right value for the index" in {

    forAll(fibonacciInput) { (index: Int, result: Int) =>

      fib(index) should be(result)

    }
  }

  it should "throw an exception when a negative index is passed" in {

    an [IllegalArgumentException] should be thrownBy fib(-1)

  }

  "The isOrdered function" should "correcty detected ordered array" in {

    val compareFunction: (Int, Int) => Boolean = (a: Int, b: Int) => {
      a <= b
    }

    forAll(sortedInput) { (array, result) => isSorted(array, compareFunction) should be(result) }

    //can be called using anonymous function (a:Int,b:Int) => a < b) as param inSorted
    // forAll(sortedInput) { (array, result) => isSorted(array, (a: Int, b: Int) => a < b) should be(result) }
  }

  "The function composition" should "correctly compose two function" in {

    val doubleFunction = (a: Int) => (a * 2)
    val addTenFunction = (a: Int) => (a + 10)

    compose(doubleFunction, addTenFunction)(2) should be(24)

  }


}
