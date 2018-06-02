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
    val second = tuple._2
    val third = tuple._3
  }

  trait intFixture extends MonoidFixture[Int] {
    val tuple = (1, 2, 3)
    val first = tuple._1
    val second = tuple._2
    val third = tuple._3
  }

  trait ListFixture extends MonoidFixture[List[Int]] {
    val tuple = (List(1), List(2), List(3))
    val first = tuple._1
    val second = tuple._2
    val third = tuple._3
  }

  trait endoFixture extends MonoidFixture[Int => Int] {
    val tuple = ((a:Int) => a, (a:Int) => a*2, (a:Int) => a+1)
    val first = tuple._1
    val second = tuple._2
    val third = tuple._3
  }

  /**
    * Generic function that tests the rules of a Monoid.
    *
    * @tparam A the monoid type
    */
  def testMonoidRules[A](tuple: (Monoid[A], MonoidFixture[A], (A, A) => A)): Unit = {
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

    val tuple = (Monoid.intAddition, new intFixture {}, (a: Int, b: Int) => a + b)

    testMonoidRules[Int](tuple)

  }

  /*The resut is a function I can't test the equality in this way
  ignore should MonoidRules in {

    val endoMonoidIntToInt = Monoid.endoMonoid[Int]

    val tuple = (endoMonoidIntToInt, new endoFixture {}, (a: Int => Int , b: Int => Int) => a.compose(b))

    testMonoidRules[Int => Int](tuple)


  }
  */

  "Monoid" should "use op and zero in a fold" in {

    val listMonoidString = listMonoid[String]

    val list = List("hi", "my", "name", "is")

    list.foldLeft(stringMonoid.zero)(stringMonoid.op) should be("himynameis")

  }

  "Monoid" should "have the same value if used on fold left or fold right" in {

    val listMonoidString = listMonoid[String]

    val list = List("hi", "my", "name", "is")

    val left = list.foldLeft(stringMonoid.zero)(stringMonoid.op)
    val right = list.foldRight(stringMonoid.zero)(stringMonoid.op)

    left should be(right)

  }

  "Concatenate" should "concatenate a List[A] using the monoid[A]" in {

    val concatenated = Monoid.concatenate(List("hi", "my", "name", "is", "luca"), stringMonoid)

    concatenated should be("himynameisluca")

  }

  "FoldMap" should "fold a list[A] using the Monoid[B] given a function f:A => B" in {

    val folded = Monoid.foldMap(List("1", "2", "3", "4", "5"),Monoid.intAddition)(a => a.toInt)

    folded should be(15)

  }

  "FoldRight via FoldMap" should "fold a list[A]" in {

    val listMonoidString = listMonoid[String]

    val list = List("hi", "my", "name", "is")

    Monoid.foldRight(list,stringMonoid.zero)(stringMonoid.op) should be("himynameis")

  }

  "The wod count monoid" should "count the words in a string using fold" in {

    Monoid.wordCount("hi my name is luca how are you ciao") should be(9)
    Monoid.wordCount("") should be(0)
    Monoid.wordCount("this is another string for trying .") should be(6)
    Monoid.wordCount("                   ") should be(0)

  }


}