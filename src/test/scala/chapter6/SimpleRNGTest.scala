package chapter6

import org.scalatest.{FlatSpec, Matchers}


class SimpleRNGTest extends FlatSpec with Matchers {

  "SimpleRNG" should "always generate the same random number, when created with the same state" in {

    (0 to 10000).foreach((x) => {

      val rng = SimpleRNG(x)
      val rng2 = SimpleRNG(x)

      rng.nextInt._1 should be(rng2.nextInt._1)
      rng.nextInt._2 should be(rng2.nextInt._2)
      rng.nextInt._2.nextInt._1 should be(rng2.nextInt._2.nextInt._1)

    })
  }

  "nonNegativeInt" should "always generate a non negative randomNumber, and need to be always the same with the same seed" in {

    (0 to 10000).foreach((x) => {
      val rng = SimpleRNG(x)
      val rng2 = SimpleRNG(x)

      SimpleRNG.nonNegativeInt(rng)._1 > 0 should be(true)
      SimpleRNG.nonNegativeInt(rng2)._1 > 0 should be(true)
      SimpleRNG.nonNegativeInt(rng)._1 should be(SimpleRNG.nonNegativeInt(rng2)._1)

    })
  }

  "double" should "generate a double value between 0 and 1 excluded" in {

    (0 to 10000).foreach((x) => {

      val rng = SimpleRNG(x)
      val rng2 = SimpleRNG(x)
      val nextDouble = SimpleRNG.double(rng)._1
      val nextDouble2 = SimpleRNG.double(rng2)._1
      (nextDouble >= 0 && nextDouble < 1) should be(true)
      (nextDouble2 >= 0 && nextDouble2 < 1) should be(true)
      nextDouble should be(nextDouble2)

    })

  }

  "ints" should "create always the same list when the same initial state is used" in {

    (0 to 1000).foreach((x) => {

      val rng = SimpleRNG(x)
      val rng2 = SimpleRNG(x)
      val nextList = SimpleRNG.ints(x)(rng)._1 //create a list of int increased
      val nextList2 = SimpleRNG.ints(x)(rng2)._1
      nextList should be(nextList2)

    })
  }

  "nonNegativeEvent" should "create only non negative even" in {

    (0 to 10000).foreach((x) => {

      val rng = SimpleRNG(x)
      val rng2 = SimpleRNG(x)
      val nextDouble = SimpleRNG.nonNegativeEven(rng)._1
      val nextDouble2 = SimpleRNG.nonNegativeEven(rng2)._1
      (nextDouble % 2 == 0) should be(true)
      (nextDouble2 % 2 == 0) should be(true)
      nextDouble should be(nextDouble2)

    })
  }

  "map" should "map the element produced from the other function according to the function providev" in {

    (0 to 10000).foreach((x) => {

      val rng = SimpleRNG(x)
      val rng2 = SimpleRNG(x)
      val next = SimpleRNG.map(SimpleRNG.int)(_ * 2)
      next(rng)._1 should be(SimpleRNG.int(rng)._1 * 2)

    })
  }

  "map2" should "should be able to combine two result" in {

    val seed = 15

    val rng = SimpleRNG(seed)

    //combining the result
    val map2 = SimpleRNG.map2(SimpleRNG.int, SimpleRNG.int)((_, _))

    val next = map2(rng)
    next._1._1 should be(SimpleRNG.int(rng)._1)
    next._1._2 should be(SimpleRNG.int(SimpleRNG.int(rng)._2)._1)

    /** Here I'm reproducing what is happening inside the map2, but i
      * maybe is not the right solution to test in this way
      */

  }


  "sequence" should "convert a list of rand in a Rand[List[A]]" in {

    val seed = 15
    val rng = SimpleRNG(seed)

    val list = List.fill(10)(SimpleRNG.int) //created a list with ten functions

    val sequenced = SimpleRNG.sequence(list)

    sequenced(rng)._1(0) should be(list(0)(rng)._1)
    sequenced(rng)._1(1) should be(list(1)(list(0)(rng)._2)._1)

  }



}
