package chapter10

import chapter7.Par
import chapter7.Par.Par

import scala.collection.immutable

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

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)

    override def zero: A => A = (a) => a
  }

  /**
    * The method concated a list of A using the monoid instance for A
    *
    * @return the concatenated list
    */
  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  /**
    * if our list has an element type that doesn’t have a Monoid instance? Well, we can always map
    * over the list to turn it into a type that does:
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    concatenate(as.map(f), m)
  }

  /** Exercise 10.6
    * Hard: The foldMap function can be implemented using either foldLeft or fold-Right.
    * But you can also write foldLeft and foldRight using foldMap! Try it.
    * Signature of the original foldRight of list
    * override def foldRight[B](z: B)(op: (A, B) => B): B =
    *     reverse.foldLeft(z)((right, left) => op(left, right))
    */
  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = {

    foldMap(list, endoMonoid[B])(f.curried)(z)

  }

  /**
    * Exercise 10.7
    * Implement a foldMap for IndexedSeq.[4] Your implementation should use the strategy of splitting the sequence in two, recursively processing each half, and then adding the answers together with the monoid.
    * *
    * 4 Recall that IndexedSeq is the interface for immutable data structures supporting efficient random access. It also has efficient splitAt and length methods.
    *
    **/

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val divided = v.grouped(2).toList
    val partialResult0 = Monoid.foldMap(divided(0).toList, m)(f)
    val partialResult1 = Monoid.foldMap(divided(1).toList, m)(f)
    m.op(partialResult0, partialResult1)
  }

  /**
    * Hard: Also implement a parallel version of foldMap using the library we developed in chapter 7.
    * Hint: Implement par, a combinator to promote Monoid[A] to a Monoid [Par[A]],[5] and then
    * use this to implement parFoldMap.
    **/

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  /**
    * Exercise 10.10
    * Write a monoid instance for WC and make sure that it meets the monoid laws.
    **/

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = a1 match {
      case Stub(x) => a2 match {
        case Stub(y) => Stub(x + y)
        case Part(lStub, words, rStub) => Part(x + lStub, words, rStub)
      }
      case Part(lStub, words, rstub) => a2 match {
        case Stub(y) => Part(lStub, words, rstub + y)
        case Part(lStub2, words2, rStub2) => Part(lStub, words + words2 + 1, rStub2)

      }
    }

    override def zero: WC = Stub("")
  }

  def wordCount(stringToCount: String): Int = {

    def twoHalves(stringToDivide: String): Seq[String] = {
      val length = stringToDivide.length
      List(stringToDivide.substring(0, length/2), stringToDivide.substring(length / 2, stringToDivide.length))
    }

    val quarter = twoHalves(stringToCount).flatMap(twoHalves)

    val wcList = quarter.map(string => WC.parse(string)).toList

    val res = foldRight(wcList,wcMonoid.zero)(wcMonoid.op)

    val wordNumber = res match {
      case x : Stub => stubToWords(x)
      case x : Part => partToWords(x)
    }
    wordNumber
  }

  val stubToWords = (stub: Stub) => if (isWord(stub.chars)) 1 else 0
  val partToWords = (part: Part) => stubToWords(Stub(part.lStub)) + stubToWords(Stub(part.lStub)) + part.words


  def isWord(str: String) = {
    str != ""
  }
}


sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {

  def parse(string: String): WC = {
    val split = string.split(" ")

    val wc = split.size match {
      case 0 => Stub("")
      case 1 => Stub(split(0))
      case _ => Part(split(0), split.length - 2, split(split.length-1))
    }
    wc
  }



}

















