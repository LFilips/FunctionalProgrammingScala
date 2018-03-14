package chapter5

import Stream._

trait Stream[+A] {

  /**
    *
    * Converts a Stream to a List
    *
    */
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(head, tail) => head() :: tail().toList
  }

  /**
    * Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.
    */

  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
    case Cons(head, _) if n == 1 => cons(head(), empty)
    case Empty => Empty
  }

  def take2(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
    case _ => Empty
  }


  def drop(n: Int): Stream[A] = n match {
    case 0 => this
    case x => this match {
      case Cons(_, tail) => tail().drop(x - 1)
      case Empty => Empty
    }
  }

  /**
    * Exercise 5.3
    * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
    */

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
    case _ => Empty
  }

  /**
    * Exercise 5.4
    * Implement forAll, which checks that all elements in the Stream match a given predicate. Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    *
    */

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(head, tail) => p(head()) && tail().forAll(p)
    case Empty => true
  }

  /**
    * Exercise 5.5
    * Use foldRight to implement takeWhile.
    *
    */
  def takeWhileFolded(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((x,y) => if (p(x)) cons(x,y) else Empty )
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
    case Empty => z
  }

  /**
    * Exercise 5.6
    * Hard: Implement headOption using foldRight. Returns an Option with the head of the stream if present, otherwise none.
    */

  //this is a normal headOption
  def headOption(): Option[A] = this match {
    case Cons(head, _) => Some(head())
    case Empty => None
  }

  def headOptionFoldRight(): Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a)) //the key of this exercise was to force the type of the None to an option?
  }


  /**
    * Exercise 5.7
    * Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.
    *
    */
  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)
  }

  def append[B >: A](toAppend: => Stream[B]): Stream[B] = {
    foldRight(toAppend)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => f(a).append(b))
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  /**
    * This is a special constructor which uses lazy head and tail
    */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  /**
    * Exercise 5.8
    * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
    *
    */

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  /**
    * Exercise 5.9
    * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.[7]
    */
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }


  /**
    *
    * Exercise 5.10
    * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    *
    */
  def fibs(): Stream[Int] = {

    def fibs(x: Int, y: Int): Stream[Int] = {
      cons(x + y, fibs(y, x + y))
    }

    cons(0, cons(1, fibs(0, 1)))
  }


  /**
    * Exercise 5.11
    * Write a more general stream-building function called unfold. It takes an initial state, and a function for producing both the next state and the next value in the generated stream.
    *
    */

  /**
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z).map((t) => t._1) match {
    case None => empty[A]
    case Some(x) => cons(x, unfold(f(z).map((t) => t._2))(f))
  }*/

}


