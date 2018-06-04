package chapter12

import chapter11._

trait Applicative[F[_]] extends Functor[F] {
  //primitive combinator are unit and map2
  def unit[A](a: A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  //derived combinator
  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))

  /**
    * apply function can be another combinator with unit, but here will be implemented in terms of map2
    * The notation (f, x) => f(x) is hard to understand. Since in the map I have F[A=>B] and an F[A]
    * to have as result an F[B] I need a function to move from F[A=>B] & F[A] to F[B]. Since the first
    * one is fuction A=>B the second one will be a param for the first function. The result is a function
    * (f,x) => f(x) where f is a A => B and x is a X. I can use the underscore notation for that with this
    * _(_) but is unreadable.
    */
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((f: A => B, x: A) => f(x))


  def map3[A, B, C, D](fa: F[A],
                       fb: F[B],
                       fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }


  def map4[A, B, C, D, E](fa: F[A],
                          fb: F[B],
                          fc: F[C],
                          fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

}

/** Here i will define real instance of applicative **/
object Applicative {

  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def unit[A](a: A): Validation[E, A] = Success(a) //only success? not sure about that
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      fa match {
        case fa@Failure(ahead, atail) => fb match {
          case Failure(bhead, btail) => Failure(bhead, ahead +: (btail ++ atail)) //merging error list and keeping errors
          case Success(x) => fa
        }
        case fa@Success(a) => fb match {
          case fbFail@Failure(head, tail) => fbFail
          case fbSucc@Success(b) => Success(f(a, b))
        }
      }
    }
  }

}


sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]