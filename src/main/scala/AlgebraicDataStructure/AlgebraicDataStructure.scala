package AlgebraicDataStructure


/**
  * Across the book there are several definition of algebraic data structure like functor, applicative, monoid and monad
  * This class has the scope of putting all together to see the difference and understand it better.
  * For any algebraic data structure there are primitive combinator that are the only needed for representing that
  * data type. Then we can derive others.
  */
object AlgebraicDataStructure {

  /**
    *
    * A functor is an alebraic data structure that contains the map operation
    */
  trait Functor[F[_]] {

    /** Map is the only primitive combinator of the functor **/
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

    /** flatMap and Unit are the primitive combinator for the Monad**/
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

    //def apply[A](a: => A): M[A] = unit(a)
  }

  /**
    *
    */
    trait Monoid[A] {
      def op(a1: A, a2: A): A
      def zero: A
    }

}
