package chapter11

import org.scalatest.{FlatSpec, Matchers}
import chapter11.Monad._

class MonadTest extends FlatSpec with Matchers{

  "The monad trait" should "provide an instance for listMonad with unit map, flatmap and map2 defined" in {
    listMonad.unit(1).map(_*2) should be(List(2))
  }

  it should "have a monad instance for int" in {
    val res = optionMonad.map(Some(1))(_*2)
  }

  "the identity monad" should "have map and flatmap" in {

    Id("hello").map(s => s.toUpperCase).flatMap(s => Id(s.substring(0,3))) should be(Id("HEL"))

  }

  "the identity monad" should "have the for comprehension" in {

    val forcComprehensionRes = for {
      a <- Id("Hello")
      b <- Id(" Monad")
    } yield a+b
    //this is equivalent to a.flatMap(b.flatMap(a+b)
    forcComprehensionRes should be(Id("Hello Monad"))
  }

}
