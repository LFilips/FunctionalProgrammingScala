package chapter11

import chapter6.State

/**
  *
  * A functor is an alebraic data structure that contains the map operation
  */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

/**
  * The monad is more powerful that the function, because it has unit and flatMap.
  */
trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    lma.foldRight(unit(List()): M[List[A]])((ma, b) => map2(ma, b)((ma, mb) => List(ma) ::: mb))
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] = {
    sequence(la.map(a => f(a)))
  }

}

object Monad {

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }
  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  }

  /**
    * Hard: State looks like it would be a monad too, but it takes two type arguments and you need a type
    * constructor of one argument to implement Monad. Try to implement a State monad, see what issues you run into, and think about possible solutions. We’ll discuss the solution later in this chapter.
    */

  //type State[S,A] = S => (S, A)  //State type, a function that brings from a state to a new state and a value
  type IntState[A] = State[Int, A]

  val intStateMonad: Monad[IntState] = new Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State(s => (a, s))

    override def flatMap[A, B](ma: IntState[A])(f: A => IntState[B]): IntState[B] = ma.flatMap(f)
  }

  def stateMonad[S] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

}

case class Id[A](value: A) {
  def map(f: A => A): Id[A] = flatMap(a => Id(f(a)))

  def flatMap(f: A => Id[A]): Id[A] = f(value)
}

// todo try to have a an application of the reader monad and see what can do
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(st.run(r)).run(r))
  }
}



