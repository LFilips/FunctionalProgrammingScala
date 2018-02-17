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
    *
    * @param list
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def foldRightUsingFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B =  {
    val g: (B, A) => B = (a,b) => f(b, a)
    foldLeft(list,z)(g)
  }

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B  =  {
    foldRight(as,z)((a,b) => f(b, a))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match{
    case Nil => z
    case Cons(head,tail) => foldLeft(tail,f(z,head))(f)
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
    foldLeft(list,0)(_ + _)
  }

  def productFoldedLeft(list: List[Double]) = {
    foldLeft(list, 1.0)(_ * _) //Shorthand for (x,y) => x*y
  }

  def lengthFoldedLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((x, _) => 1 + x)
  }


  def reverse[A](list: List[A]) : List[A] = {

    @annotation.tailrec
    def recursiveReverse[A](list: List[A],buffer: List[A]) : List[A] = list match {
      case Nil => buffer
      case Cons(head,tail) => recursiveReverse(tail,Cons(head,buffer))
    }
    recursiveReverse(list,Nil)

  }

  //todo
  def append[A](list: List[A]) : List[A] = {
    Nil
  }


}

object RunExample extends App {

  import chapter3.List._

  val result = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) //It applies the constructor to each element of the list

  println(result == List(1, 2, 3)) //True, I get the same list thati I gave as input

}

