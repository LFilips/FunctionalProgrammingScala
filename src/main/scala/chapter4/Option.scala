package chapter4

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def orElse[B >: A](default: => Option[B]): Option[B] = this match {
    case None => default
    case Some(x) => Some(x)
  }

  /**
    *
    * Apply f which may fail, to the option if not None, the difference with map is that this operation can create
    * a None, because f: A => Option[B]
    *
    */
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(x) => if (f(x)) Some(x) else None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  /**
    * THIS DON'T WORK
    *
    * I have a list of option tha need to become an option containing a list
    *
    * Step 1: Using pattern matching to decompose the list and access to head and tail
    * Step 1: Use map on the head if the list for acessing to the null
    * Step 2:
    *
    *
    *
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    sequenceHelper(a) match {
      case Nil => None
      case list => Some(list)
    }
  }


  def sequenceHelper[A](a: List[Option[A]]): List[A] = a match {
    case Nil => Nil
    case None :: tail => Nil
    case Some(x) :: tail => List(x) ::: sequenceHelper(tail)
  }


  /**
    * The difference is that with flatMap I'm able to create my own option, instead with map i cant
    *
    * So for going from the realm of a Some(Int) to the real of the Some(List())
    * Is necessary to use the flatMap operation and provide the construction of an Option with the list inside
    *
    *
    * It I explode the function calls for List(Some(1),Some(2)):
    *
    * head.flatMap((1) => sequenceUsingFlatMap(List(Some(2)).map((2) => 1 :: 2))
    *
    * In the end I have a Some(Nil).map((Nil) => (list that will be created recursiveli) :: Nil)
    *
    */

  def sequenceUsingFlatMap[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => head.flatMap((x) => sequenceUsingFlatMap(tail).map((y) => x :: y))
  }

}


object Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2
    * Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the mean of
    *math.pow(x - m, 2) for each element x in the sequence.
    */

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((mean) => Some((xs.fold(0.0)((x, y) => Math.pow(y - mean, 2.0) + x)))).map((value) => value / xs.size)
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = (x) => x.map(f)

  val absLifted: Option[Double] => Option[Double] = lift(Math.abs)

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {
      case e: Exception => None
    }
  }

  /**
    * Exercise 4.3
    * Write a generic function map2 that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too. Here is its signature:
    */

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
    case None => None
    case Some(a) => b.map((b) => f(a, b))
  }


}

