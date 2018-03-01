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
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  /**
    *
    * orElse method, it should give the value if present otherwise another
    *
    */
  def orElse[E, B >: A](e: => Either[E, B]): Either[E, B] = this match {
    case Right(x) => Right(x)
    case Left(_) => e
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => f(x)
    case Left(x) => Left(x)
  }

  //Previous signature def map2[E, B, C](b: Either[E, B])(f: (A, B) => C): Either[E, C], from which I get the same error, suspicios shadowing
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(a) => Left(a)
    case Right(a) => b.map((b) => f(a, b))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]




