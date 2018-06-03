package chapter10

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}


/**
  * Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream]. Remember that foldRight, foldLeft, and foldMap can all be implemented in terms of each other, but that might not be the most efficient implementation.
  **/

object foldableList extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
    as.foldRight(z)(f)
  }

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    as.foldLeft(z)(f)
  }

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
    as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
  }

}
