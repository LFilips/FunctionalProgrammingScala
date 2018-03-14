package chapter3


import chapter3.List._
import org.scalatest.{FlatSpec, Matchers}


class FunctionalDataStructureTest extends FlatSpec with Matchers {

  "The tail method" should "return a list without the head" in {

    val list = Cons(1,Cons(2,Cons(3,Nil)))

    val result = tail(list)

    result should be(List(2,3))

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

  "The append method" should "append two list" in {

    val list = Cons(1.0,Cons(2.0,Cons(3.0,Cons(4.0,Nil))))

    val listToAppend = Cons(1.0,Cons(2.0,Cons(3.0,Cons(4.0,Nil))))

    val result = append(list,listToAppend)

    result should be(Cons(1.0,Cons(2.0,Cons(3.0,Cons(4.0,Cons(1.0,Cons(2.0,Cons(3.0,Cons(4.0,Nil)))))))))
  }

  "The concatenated method" should "flatten a list of list" in {

    val list = List(List(1,2,3),List(1,2,3))

    val result = concatenates(list)

    result should be(List(1,2,3,1,2,3))
  }

  "The map method" should "apply the provided function to each element of the list" in {

    val list = List(1,2,3,4,5,6)

    val result = map(list)((x) => x*2)

    result should be(List(2,4,6,8,10,12))


  }

  "The filter method" should "filter out the elements in the list that doesn't match the predicate" in {

    val list = List(1,2,3,4,5,6)

    val result = filter(list)((x) => x%2==0)

    result should be(List(2,4,6))

  }

  "The flatMap method" should "emit a list for each element and then flatten it" in {

    val result = flatMap(List(1,2,3))(i => List(i,i))

    result should be(List(1,1,2,2,3,3))

  }

  "The filter method implemented with flatMap" should "filter out the elements in the list that doesn't match the predicate" in {

    val list = List(1,2,3,4,5,6)

    val result = filterWithFlatMap(list)((x) => x%2==0)

    result should be(List(2,4,6))

  }

  "The size method of the three" should "count correctly the number of node and leaves" in {

    val tree = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))

    /*
          branch
     branch     branch
    1     2    3     4

    The total should be 7 nodes
     */

    val result = Tree.size(tree)

    result should be(7)

  }

  "The maximum method of the three" should "give the max in the tree" in {

    val tree = Branch(Branch(Leaf(1),Leaf(7)),Branch(Leaf(3),Leaf(4)))

    val result = Tree.maximum(tree)

    result should be(7)

  }

  "The map method of the three" should "apply the given function to each element" in {

    val tree = Branch(Branch(Leaf(1),Leaf(7)),Branch(Leaf(3),Leaf(4)))

    val result = Tree.map(tree)(_*2.0)

    result should be(Branch(Branch(Leaf(2.0),Leaf(14.0)),Branch(Leaf(6.0),Leaf(8.0))))

  }

  "The depth method of the three" should "return the depth of the tree" in {

    val tree = Branch(Branch(Leaf(1),Leaf(7)),Branch(Branch(Leaf(1),Leaf(7)),Leaf(4)))

    val result = Tree.depth(tree)

    result should be(4)

  }

  "The maximumFolded method of the three" should "give the max in the tree" in {

    val tree = Branch(Branch(Leaf(1),Leaf(7)),Branch(Leaf(3),Leaf(4)))

    val result = Tree.maximumFolded(tree)

    result should be(7)

  }

}
