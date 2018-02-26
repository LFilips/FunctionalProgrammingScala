package chapter3

sealed trait List[+A] // `List` data type, parameterized on a type, `A`

case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 3.2
    * Implement the function tail for removing the first element of a List. Note that the function takes constant time.
    * What are different choices you could make in your implementation if the List is Nil? We’ll return to this
    * question in the next chapter.
    *
    */
  def tail[A](list: List[A]) = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  /**
    * Exercise 3.3
    * Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
    */

  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => Cons(newHead, tail)
  }

  /**
    *
    * Exercise 3.4
    * Generalize tail to the function drop, which removes the first n elements from a list. Note that this function takes time proportional only to the number of elements being dropped—we don’t need to make a copy of the entire List.
    * *
    *
    */
  def drop[A](l: List[A], n: Int): List[A] = {

    @annotation.tailrec
    def recursiveDropper(l: List[A], n: Int): List[A] = n match {
      case 0 => l
      case n => recursiveDropper(tail(l), n - 1)
    }

    l match {
      case Nil => Nil
      case Cons(_, _) => recursiveDropper(l, n)
    }

  }

  /**
    *
    * Exercise 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    *
    */
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l
  }

  /**
    * Exercise 3.6
    * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last
    * element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can’t this function be implemented in constant time like tail? */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil //giving back the empty list, in this way when I have the last element it will be truncated
    case Cons(head, tail) => Cons(head, init(tail))
  }


  /**
    *
    * Not an exercise, is just the dropWhile with two param list so the type of the function can be inferrred
    *
    */
  @annotation.tailrec
  def dropWhileInferred[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhileInferred(tail)(f) else l
  }

  /**
    * The fold right function is used to apply a function recursively to the element of the list.
    * The input function is of type (A,B) => B, so an example of stack trace of it is the following:
    * f(head, f(head, f(head, z)), in case the function is a sum (_+_)in a list of Int, with 0 as z
    * So is called in this way     foldRight(list, 0)((x,y) => x+y)
    *
    * sum(1, sum(2, sum(3, 0))) -> 6
    *
    */
  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def foldRightUsingFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    val g: (B, A) => B = (a, b) => f(b, a)
    foldLeft(reverse(list), z)(g) //Need to be reverted!!
  }

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, z)((a, b) => f(b, a))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  /*
  def reverseFolded[A](list: List[A]) : List[A] = list match {
    case Nil => Nil
    case Cons(head,tail) => foldLeft(tail,head)((list,elem) => List(list,elem))
  }*/

  def sumFolded(list: List[Int]) = {
    foldRight(list, 0)(_ + _) // shorthand for (x,y) => x+y
  }

  def productFolded(list: List[Double]) = {
    foldRight(list, 1.0)(_ * _) //Shorthand for (x,y) => x*y
  }

  /**
    * Exercise 3.7
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
    * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list. This is a
    * deeper question that we’ll return to in chapter 5.
    * *
    * Answer:
    * Can be done, making some changes to the foldRight function
    */

  def productFoldedShortCircuited(list: List[Int]) = {
    foldRight(list, 1)(_ * _) //Cannot be impelement without making changes in folding method
  }

  /**
    * Exercise 3.9
    * Compute the length of a list using foldRight.
    */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => 1 + y)
  }

  def sumFoldedLeft(list: List[Int]) = {
    foldLeft(list, 0)(_ + _)
  }

  def productFoldedLeft(list: List[Double]) = {
    foldLeft(list, 1.0)(_ * _) //Shorthand for (x,y) => x*y
  }

  def lengthFoldedLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((x, _) => 1 + x)
  }


  def reverse[A](list: List[A]): List[A] = {
    @annotation.tailrec
    def recursiveReverse[A](list: List[A], buffer: List[A]): List[A] = list match {
      case Nil => buffer
      case Cons(head, tail) => recursiveReverse(tail, Cons(head, buffer))
    }
    recursiveReverse(list, Nil)
  }

  /**
    *
    * Append a list to another
    *
    * @param list         the base list
    * @param listToAppend the list to appen
    * @tparam A the list
    * @return
    */
  def append[A](list: List[A], listToAppend: List[A]): List[A] = list match {
    case Nil => listToAppend
    case Cons(head, tail) => Cons(head, append(tail, listToAppend))
  }

  /**
    * Exercise 3.15
    * Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total
    * length of all lists. Try to use functions we have already defined.
    */


  def concatenates[A](list: List[List[A]]): List[A] = {

    @annotation.tailrec
    def concatenatesHelper(list: List[List[A]], flattenedList: List[A]): List[A] = list match {
      case Nil => flattenedList
      case Cons(_, Nil) => flattenedList
      case Cons(head, Cons(secondHead, tail)) => concatenatesHelper(tail, append(head, secondHead))
    }

    concatenatesHelper(list, Nil)
  }

  /**
    * Exercise 3.16
    * Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a pure function that returns a new List!)
    *
    * I should use the fold list to preform this operation
    *
    * f(head, f(head, f(head, z)) is how the function will be developed
    *
    * The z element will be for sure a Nil list
    *
    * f(head,f(head,f(head,Nil)))
    *
    * I can piggyback the append function
    *
    * This structure is the same of the constructor that I've got for the list Cons(1,Cons(2,Cons(3,Nil)
    *
    * The only think is that each element need to be the element on the input list +1
    *
    * so the natural definition for the add1 method is something like: foldRight(list)((x,y) => Cons(x+1,y)
    *
    */

  def add1ToAll(list: List[Int]): List[Int] = {
    foldRightUsingFoldLeft(list, Nil: List[Int])((h, t) => Cons(h + 1, t))

  }

  def doubleToString(list: List[Double]): List[String] = {
    foldRightUsingFoldLeft(list, Nil: List[String])((h, t) => Cons(h.toString, t))
  }


  /**
    * Exercise 3.18
    * Write a function map that generalizes modifying each element in a list while maintaining the structure of the list. Here is its signature:[12]
    *
    * 12 In the standard library, map and flatMap are methods of List.
    */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRightUsingFoldLeft(as, Nil: List[B])((head, tail) => Cons(f(head), tail))
  }

  /**
    * Exercise 3.19
    * Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].
    *
    *
    * This function can probablu be define in term of folding too
    *
    * f(head,f(head,f(head,Nil))
    *
    * The output will be a list, so the z element of the fold will a Nil list
    *
    * f(a,f(b,f(c,Nil)) what I need to do is if b doesn't satisfy the predicate this should become f(a,f(c,Nil)
    *
    * fold(list)((x,y) => if f(a) Cons(x,y) else tail
    *
    */

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRightUsingFoldLeft(as, Nil: List[A])((head, tail) => if (f(head)) Cons(head, tail) else tail)
  }

  /**
    * Write a function flatMap that works like map except that the function given will return a list instead of a single
    * result, and that list should be inserted into the final resulting list. Here is its signature:
    *
    */

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRightUsingFoldLeft(as, Nil: List[B])((head, tail) => append(f(head), tail))
  }

  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x) => if (f(x)) Cons(x, Nil) else Nil)
  }

}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //usually tree are traversed recursevily on left and right
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = {

    def recMaximum(tree: Tree[Int], max: Int): Int = tree match {
      case Leaf(x) => x
      case Branch(left, right) => recMaximum(left, max).max(recMaximum(right, max))
    }

    recMaximum(tree, Int.MinValue) //would be better to use one of the element as first max
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }


  def map[A, B](tree: Tree[A])(f: (A) => B) : Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch[B](map(left)(f),map(right)(f))
  }


  def fold[A, B](tree: Tree[A])(g: (B,B) => B)(f: A => B) : B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(g)(f),fold(right)(g)(f))
  }

  def maximumFolded(tree: Tree[Int]) : Int = {
    fold[Int,Int](tree)(_ max _)((a) => (a))
  }

}

object RunExample extends App {

  import chapter3.List._

  val result = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) //It applies the constructor to each element of the list

  println(result == List(1, 2, 3)) //True, I get the same list thati I gave as input

}

