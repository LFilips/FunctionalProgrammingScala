package chapter6

import org.scalatest.{FlatSpec, Matchers}

class StateTest extends FlatSpec with Matchers {

  behavior of "State case class"

  it should "run the function provided" in {

    val state = State(SimpleRNG.int)

    val seed = 5

    val rng = SimpleRNG(seed)

    state.run(SimpleRNG(seed)) should be(rng.nextInt)

  }

  it should "map the A value of the state in a String" in {

    val state = State(SimpleRNG.int)

    val seed = 5

    val rng = SimpleRNG(seed)

    state.map(_.toString).run(SimpleRNG(seed)) should be(rng.nextInt._1.toString,rng.nextInt._2) //recreating the state with the toString

  }

  it should "map the A value applying the function provided" in {

    val state = State(SimpleRNG.int)

    val seed = 5

    val rng = SimpleRNG(seed)

    state.map(_ * 2).run(SimpleRNG(seed)) should be(rng.nextInt._1 * 2,rng.nextInt._2) //recreating the state with * 2

  }

  it should "correcty use the map2 function to map two state and apply a function to the value" in {


    val stateA = State(SimpleRNG.int)
    val stateB = State(SimpleRNG.int)


    val seed = 5

    val startingRng = SimpleRNG(seed)

    val (a,rngA) = startingRng.nextInt

    val (b,rngB) = rngA.nextInt

    val expected = (s"""$a $b""",rngB)

    stateA.map2[Int,String](stateB)((a,b) => s"""$a $b""").run(SimpleRNG(seed)) should be(expected) //recreating the state with * 2

  }



  it should "flatMap" in {

    val state = State(SimpleRNG.int)

    val seed = 5

    val rng = SimpleRNG(seed)


    val startingRng = SimpleRNG(seed)

    val (a,rngA) = startingRng.nextInt

    val (b,rngB) = rngA.nextInt

    /**
      * I'm checking if it sums the value from the two random number generator and if passes along the state
      */
    state.flatMap((x) => State(SimpleRNG.int).map(_ + x)).run(SimpleRNG(seed)) should be(a + b,rngB)



  }

}
