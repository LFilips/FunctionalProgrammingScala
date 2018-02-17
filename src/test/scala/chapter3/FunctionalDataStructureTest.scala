package chapter3


import chapter3.List._
import org.scalatest.{FlatSpec, Matchers}


class FunctionalDataStructureTest extends FlatSpec with Matchers {

  "The tail method" should "return a list without the head" in {

    val list = Cons(1,Cons(2,Cons(3,Nil)))

    val result = tail(list)

    result should be(Cons(2,Cons(3,Nil)))

  }

  "The tail method" should "return Nil when Nil providewd" in {

    val list = Nil

    val result = tail(list)

    result should be(Nil)

  }

  "The tail method" should "return Nil when 1 Elem List provied" in {

    val list = Cons(3,Nil)

    val result = tail(list)

    result should be(Nil)

  }

  "The setHead method" should "correctly set a new head" in {

    val list = Cons(1,Cons(2,Cons(3,Nil)))

    val result = setHead(list,10)

    result should be(Cons(10,Cons(2,Cons(3,Nil))))

  }

  "The drop method" should "drop n element from the head of the list" in {

    val list = Cons(1,Cons(2,Cons(3,Nil)))

    val result = drop(list,2)

    result should be(Cons(3,Nil))

  }

  "The dropWhile method" should "drop elements until they match the predicate" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val predicate : (Int) => Boolean =  (elem) => elem < 3

    val result = dropWhile(list, predicate)

    result should be(Cons(3,Cons(4,Cons(5,Nil))))

  }

  "The dropWhile method" should "drop elements until they match the predicate defined with an anonymous function" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val result = dropWhile(list, (elem:Int) => elem < 3)

    result should be(Cons(3,Cons(4,Cons(5,Nil))))

  }

  "The dropWhile inferred method" should "drop elements until they match the predicate defined with an anonymous function without type information" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val result = dropWhileInferred(list)((elem) => elem < 3)

    result should be(Cons(3,Cons(4,Cons(5,Nil))))

  }

  "The foldRight method" should "apply the function provided to each element of the list" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val result = foldRight(list,0)((x,y) => x + y)

    result should be(15)

  }


  "The length method" should "give the right length of the list" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val result = List.length(list)

    result should be(5)

  }

  "The length method" should "give 0 for an Nil list" in {

    val list = Nil

    val result = List.length(list)

    result should be(0)

  }

  "The foldLeft method" should "apply the function provided to each element of the list" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val result = foldLeft(list,0)((x,y) => x + y)

    result should be(15)

  }

  "The foldedLeft sum" should "compute the right sum of the list" in {

    val list = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

    val result = sumFoldedLeft(list)

    result should be(15)
  }

  "The foldedLeft product" should "compute the right product of the list" in {

    val list = Cons(1.0,Cons(2.0,Cons(3.0,Cons(4.0,Nil))))

    val result = productFoldedLeft(list)

    result should be(24)
  }

  "The reverse method" should "reverse the list" in {

    val list = Cons(1.0,Cons(2.0,Cons(3.0,Cons(4.0,Nil))))

    val result = reverse(list)

    result should be(Cons(4.0,Cons(3.0,Cons(2.0,Cons(1.0,Nil)))))
  }


}
