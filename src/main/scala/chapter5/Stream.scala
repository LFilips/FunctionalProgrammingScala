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
    foldRight(Empty: Stream[A])((x, y) => if (p(x)) cons(x, y) else Empty)
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

  /**
    * Exercise 5.13
    * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll. The zipAll function should
    * continue the traversal as long as either stream has more elements—it uses Option to indicate whether each stream
    * has been exhausted.
    */

  def mapUnfold[B](f: A => B): Stream[B] = {
    //  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
    val mapping = (s: Stream[A]) => s match {
      case Cons(head, tail) => Some((f(head()), tail()))
      case Empty => None
    }
    Stream.unfold(this)(mapping) //the first state is the list, then each state is compose from the tail and the value
    // is obtained applying the map function
  }

  /** This time the state is composed from the Stream and the n value,
    * each actual element is the head of the list, untile n is greater than zero */

  def takeUnfold(n: Int): Stream[A] = {
    //  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
    val mapping = (state: (Stream[A], Int)) => state match {
      case (Cons(head, tail), n) if n > 0 => Some(head(), (tail(), n - 1))
      case (Cons(_, _), n) if n == 0 => None
      case (Empty, _) => None
    }
    Stream.unfold((this, n))(mapping)
  }


  /**
    * this time the state is composed only by the stream
    * each actual element is the head of the list, untile f(head) is true
    */
  def takeWhileUnfold(f: A => Boolean): Stream[A] = {
    val mapping = (state: Stream[A]) => state match {
      case Cons(head, tail) => if (f(head())) Some(head(), tail()) else None
      case Empty => None
    }
    Stream.unfold(this)(mapping)
  }

  /**
    *
    * Zipwith merge two stream according a merging function on two element (A,B) => C
    *
    * In this case the state will be composed by the two stream, and the actual element
    * will be the function applied on both the head
    */

  def zipWithFunctionUnfold[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = {
    val mapping = (state: (Stream[A], Stream[B])) => state match {
      case (Cons(headA, tailA), Cons(headB, tailB)) => Some((f(headA(), headB()), (tailA(), tailB())))
      //case (Empty, Cons(headB, tailB)) => Some((f(Empty, headB()), (Empty, tailB()))) //the function will handle empty stream in A
      //case (Cons(headA, tailA), Empty) => Some((f(headA, Empty), (tailA(), Empty))) //the function will handle empty stream in B
      case (_,_) => None
    }
    Stream.unfold((this, b))(mapping)
  }

  /**
    *
    * Zip two streams in one, where each element is composed by a tuple with the element in the same position of the Stream.
    *
    */
  def zipWithUnfold[B, C](b: Stream[B]) : Stream[(A,B)] = {
    val mapping = (state: (Stream[A], Stream[B])) => state match {
      case (Cons(headA, tailA), Cons(headB, tailB)) => Some((headA(), headB()), (tailA(), tailB()))
      case (_,_) => None
    }
    Stream.unfold((this, b))(mapping)
  }

  /**
    *
    * Creates a stream composed by tuples where the element are the one in the two stream in the same position. In case
    * of a stream exhaust a none is used for the element.
    *
    */

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = {
    val mapping = (state: (Stream[A], Stream[B])) => state match {
      case (Cons(headA, tailA), Cons(headB, tailB)) => Some((Some(headA()),Some(headB())), (tailA(), tailB()))
      case (Empty, Cons(headB, tailB)) => Some((None,Some(headB())),(empty,tailB()))
      case (Cons(headA, tailA), Empty) => Some((Some(headA()),None),(tailA(),empty))
      case (_,_) => None
    }
    Stream.unfold((this, s2))(mapping)
  }

  /**
    *
    *
    * Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
    * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
    *
    *
    * For first the two stream are combined in one with the zip method. I only need tuple to the smallest one
    * So Stream(1,2,3).zip( Stream(1,2)) => Stream((1,1),(2,2))
    * Then using the folding functioin testing if the element of the tuple are the same (all the result are in a AND)
    *
    * 1) The function will short circuit on the first failure because of the and
    * 2) this function don't care about the order of the operand, it check if one of the stream starts with the other
    *
    */
  def startsWith[A](s: Stream[A]): Boolean = {
    this.zipWithUnfold(s) match {
      case Empty => false
      case stream => stream.foldRight(true)((a,b) => (a._1 == a._2) && b )
    }
  }

  /**
    *
    *Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence,
    * starting with the original Stream. For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3),
    * Stream(2,3), Stream(3), Stream()).
    *
    */


  def tails: Stream[Stream[A]] = {
    unfold(this)(_ match {
      case Cons(head,tail) => Some(Cons(head,tail),tail())
      case Empty => None
    }) //I am missing the last empty stream, but this is really necessary? Can I simply append to it?
  }



  def scanRight[B](s: B)(f: (A,A) => B ) : Stream[B] = ???
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


  val ones: Stream[Int] = Stream.cons(1, ones)

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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    //matching using the function, so if the next state exist I'll do that, otherwise return an Empty stream
    case Some(_) => cons(f(z).map((t) => t._1).get, unfold(f(z).map((t) => t._2).get)(f))
    case _ => empty
  }

  /**
    * Exercise 5.12
    * Write fibs, from, constant, and ones in terms of unfold.
    */


  def onesUnfolded(): Stream[Int] = unfold(0)((s) => Some(1, 0))


  def constantUnfolded[A](a: A): Stream[A] = unfold(a)((s) => Some(a, s))

  /**
    * The generic composition of a state help to manipulation any kind of situation, what I need fot fibonacci is that  state is composed from the previous 2 value that I have,
    * I didn't get this before because all the function weren't realy requiring a state to operate
    *
    *
    */
  def fibsUnfolded(): Stream[Int] = {
    unfold((-1, -1))( //the first unfold is called with the first value, now i hacve to define the function the calculates the value from the state
      _ match {
        case (-1, -1) => Some(0, (0, 0)) //I needed this special case, otherwise the sequence will be (0,1,2 ...)
        case (0, 0) => Some(1, (0, 1)) //in case of 0,0 I can't use the fibonacci algorithm so I have to provide the right state
        case state => Some(state._1 + state._2, (state._2, state._1 + state._2))
      }
    )
  }

}


