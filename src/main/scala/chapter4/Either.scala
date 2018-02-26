package chapter4


/**
  *
  * Definition of sealed trait
  *
  * @tparam E the type in the left, usually an error
  * @tparam A the type in the right, usually the good case
  */
sealed trait Either[+E, +A] {

  /**
    *
    * Applyies the function if the either is right, otherwise no effect
    *
    * @param f
    * @tparam B
    * @return
    */
  def map[B](f: A => B) : Either[E,B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  /**
    *
    * orElse method, it should give the value if present otherwise another
    *
    */
 /* def orElse[E,A](e: => Either[E,A]) : Either[E,A] = this match {
    case Right(x) => Right(x)
    case Left(x) => e
  }

  def flatMap[E,B](f: A => Either[E,B]) : Either[E,B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }*/



}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]




