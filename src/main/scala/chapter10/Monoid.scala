package chapter10

/**
  * A monoid is an algebraic structure, it consist in:
  *
  * 1)Type A
  * 2)Associative binary operation op, which combines two values of A into one.
  * 3)A value zero: A, that is an identity for that operation
  *
  * The correct definition is that A forms a monoid under the operations defined by the Monoid[A] instance
  *
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A

}

/**
  * Exercise 10.1
  * Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
  * val intAddition: Monoid[Int]
  * val intMultiplication: Monoid[Int]
  * val booleanOr: Monoid[Boolean]
  * val booleanAnd: Monoid[Boolean]
  */


object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplicationM = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  /**
    * Exercise 10.3
    * A function having the same argument and return type is sometimes called an endofunction.[2] Write a monoid for endofunctions.
    * *
    * The Greek prefix endo- means within, in the sense that an endofunction’s codomain is within its domain.
    **/

  def endoMonoid[A] : Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    override def zero: A => A = (a) => a
  }
}
