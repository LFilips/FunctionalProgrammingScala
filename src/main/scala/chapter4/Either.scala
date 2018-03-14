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
  def orElse[EE >: E, B >: A](e: => Either[EE, B]): Either[EE, B] = this match {
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

object Either {


  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }

  /**
    * The sequence method, should create an Either that contain a list of all the value that Right in the list provided as input
    *
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case head :: tail => head.flatMap((headEither) => sequence(tail).map((tailHead) => headEither :: tailHead))
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case head :: tail => f(head).flatMap((headEither) => traverse(tail)(f).map((tailHead) => headEither :: tailHead))
  }
}




