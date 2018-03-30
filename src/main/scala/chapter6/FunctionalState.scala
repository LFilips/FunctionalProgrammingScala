package chapter6

import chapter6.SimpleRNG.Rand

trait RNG {

  /**
    *
    * Generate a RandomInteger
    *
    * @return a tuple cointaning the number and the new state
    */
  def nextInt: (Int, RNG)

}


case class SimpleRNG(seed: Long) extends RNG {
  /**
    *
    * Generate a RandomInteger, and returns the new RNG to use. This is an example of how we can handle api which mutates
    * the state, just passing the state around instead of updating it as a side effect.
    *
    * @return a tuple containing the number and the new state
    */
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 1981907 + 0x1128714) & 0xFFFFFFFFL //using the previous seed for calculating the new one
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >> 16).toInt
    (n, nextRNG)
  }

}

object SimpleRNG {
  /**
    * Exercise 6.1
    * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
    * Make sure to handle the corner case when nextInt returns Int.MinValue,
    * which doesn’t have a non-negative counterpart.
    *
    *
    * In case the Int.MIN is generated, I'm converting it to the Int.Max, but this create an uneven
    * distribution because the Int.MAX has more chance to be generated compared the other number.
    *
    *
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (nextInt, state) if nextInt == Int.MinValue => (Int.MaxValue, state)
      case (nextInt, state) if nextInt < 0 => (-nextInt, state)
      case (nextInt, state) => (nextInt, state)
    }
  }


  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /**
    * Exercise 6.2
    * Write a function to generate a Double between 0 and 1, not including 1. Note: You can use Int.MaxValue to obtain the
    * maximum positive integer value, and you can use x.toDouble to convert an x: Int to a Double.
    *
    */
  def double(rng: RNG): (Double, RNG) = {
    val (rand, rng2) = rng.nextInt
    (rand.toDouble / (Int.MaxValue - 1), rng2) //the nextInt method covers all the space till Int.max
  }

  /**
    * Exercise 6.3
    * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
    *
    *
    */

  /**
    *
    * Generates and Int,Double pair
    *
    * @param rng hte random generator state
    * @return a tuple with the pair and the new state
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = SimpleRNG.double(rng2) //using the state generated from the int generation for the double
    ((i1, i2), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((int, double), rng2) = SimpleRNG.intDouble(rng)
    ((double, int), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (first, state) = SimpleRNG.double(rng)
    val (second, state2) = SimpleRNG.double(state)
    val (third, state3) = SimpleRNG.double(state2)
    ((first, second, third), state3)
  }

  /**
    *
    * Exercise 6.4
    * Write a function to generate a list of random integers.
    *
    */

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    require(count >= 0, "Count should be greater or equal than zero")

    /**
      * I need to keep track of all the state, so I need to propagate each state that is created
      * during the process
      */
    @annotation.tailrec
    def tailedInts(count: Int, partialResult: List[Int])(rng: RNG): (List[Int], RNG) = count match {
      case 0 => (partialResult, rng)
      case _ => {
        val (int, nextState) = rng.nextInt
        tailedInts(count - 1, int :: partialResult)(nextState)
      }
    }

    tailedInts(count, List())(rng)
  }


  /**
    *
    * Type alias for the function RNG => (A, RNG), so instead of passing around this function I can use this alias for
    * defining it.
    *
    * @tparam A
    */
  type Rand[+A] = State[RNG, A]


  type State[S, +A] = S => (A, S) /** More generic definition for state **/


  /**
    *
    * This new definition actually contains two changes, one is due to the type alias
    * and the other is that I'm just defining a val within inside a function
    *
    */
  val int: Rand[Int] = _.nextInt

  def intDef(rng: RNG): Rand[Int] = _.nextInt

  /**
    *
    * As said above here I'm using the type alias but I'm defining
    * a function with returns a function, which is much different compared to what
    * I've did before, where the return type was a tuple.
    * So the big difference now is not the type alias, but the fact that i'm using a function
    * as return type
    *
    */

  def unit[A](a: A): Rand[A] = (rng) => (a, rng)


  /**
    * This function unit can be expanded in this way:
    */
  def unitExpanded[A](a: A): (RNG) => (A, RNG) = (rng) => (a, rng)

  /**
    *
    * So the real change of point of view in not dictated by the type alias per se, but rather from
    * starting using function as output of our function, this makes sense because anytime I needed to provide
    * a RNG object as a param so is enough to just define the function and then when really needed use it
    *
    */

  /**
    *
    *
    * The map function gives back a function of type (RNG) => (B,RNG), it has as input a function
    * of type Rand[A] and then a function wich transform the element of the function from A to B.
    * This can be particularly good for applying a simple function to operation as int
    *
    */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    (rng) => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(x => x - x % 2)

  /**
    * Exercise 6.5
    * Use map to reimplement double in a more elegant way. See exercise 6.2.
    **/

  def doubleWithMap: Rand[Double] = map(int)((x) => x.toDouble)

  /**
    * The map function isn't powerful enough to express all the function that i've defined before
    *
    * Exercise 6.6
    * Write the implementation of map2 based on the following signature. This function takes two actions,
    * ra and rb, and a function f for combining their results, and returns a new action that combines them:
    *
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (rng) => {
      val aTuple = ra(rng)
      val bTuple = rb(aTuple._2) //I need to be careful while passing the state
      (f(aTuple._1, bTuple._1), bTuple._2)
    }
  }

  /** Through the use of map2 i can combine two generator in the same way i've in the test */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  /**
    * Exercise 6.7
    * Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
    * Implement sequence for combining a List of transitions into a single transition.
    * Use it to reimplement the ints function you wrote before. For the latter,
    * you can use the standard library function List.fill(n)(x) to make a list with x repeated n times.
    *
    */


  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

    /**
      * The sequence function should be able to convert a List of functions
      * List[(RNG) =>  (A,RNG)] to a (RNG) => (List(A),RNG)
      *
      * So the idea is that a list of transition will be applied giving back all the value
      * obtainined in the list and then the last state
      *
      * A good way for combining the elment of the function is the folding
      *
      */
    fs.foldRight(unit(List()): Rand[List[A]])((randA, randB) =>
      map2(randA, randB)(_ :: _))
  }


  val nonNegativeIntFunctional: Rand[Int] = (rng) => nonNegativeInt(rng)


  /**
    * Creates a function which can be use to create a non negative Int less then a certain value
    *
    * @param n the upper bound
    * @return the Rand[Int]
    */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    //map(nonNegativeIntFunctional)(_ % n) //this is the first naive implementation, but lead to an uneven distribution of value
    rng =>
      val (i, rng2) = nonNegativeIntFunctional(rng)
      val mod = i % n //getting the mod
      if (i + (n - 1) - mod >= 0) {
        /**
          * discarding any number that is higher of the largest multiple of n
          * if this value is more thant zero means that we did't had overflow
          *
          * An example is the number if is generate the number: 2147483630 with n = 500
          *
          * i = 2147483630
          * mod = 2147483630 % 500 = 130
          *
          * 2147483630 + (499) is already an overflow so the number will became negative
          *
          *
          */
        (mod, rng2)
      }
      else {
        nonNegativeLessThan(n)(rng)
      }
  }

  /**
    * Exercise 6.8
    * Implement flatMap, and then use it to implement nonNegativeLessThan.
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)

      /**
        * g(a) will produce a Rand[B], but since this is the block of the
        * rng => we need to access to the (A,RNG) of it, so we use the state
        * that we have as input and then we pass the new one produced from g
        * in the output tuple
        */

    }
  }

  def nonNegativeLessThanFlatMap(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  /**
    * Map with flatmap, this is the original map implementation
    **/
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)((a) => (rng) => (f(a), rng))
  }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)((a) => (rng) => (f(a, rb(rng)._1), rng))
  }

}

case class State[S, +A](run: S => (A, S)) {


  def map[B](f: A => B): State[S, B] = {
    /**
      * I'm invoking the map to a state, which is not evaluating the state itself, what we want is
      * when the state will be evaluated the f funciton will be called on the A to become a B
      */
    State((state) => (f(this.run(state)._1), this.run(state)._2))
  }


  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    State((state) => {
      val (a, rng) = this.run(state)
      val (b, rngB) = rb.run(rng)
      (f(a, b), rngB)
    })
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = {
    State((state) => {
      val (a, rng) = this.run(state)
      g(a).run(rng) //passing the state along as we did in rand flatMap
    })
  }

}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  /**
    * Exercise 6.11
    * Hard: To gain experience with the use of State, implement a finite state automaton that models a simple candy
    * dispenser. The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
    * It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many
    * coins it contains.
    */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
    case head :: tail =>
      State((state) => State.simulateMachine(tail).run(calculateNextState(head).run(state)._2))
    case Nil =>
      //When the input are finished I can return the chain of state
      State((state) => ((state.candies, state.coins), Machine(state.locked, state.candies, state.coins)))
  }


  private def calculateNextState(input: Input): State[Machine, (Int, Int)] = {
    State(input match {
      case Coin =>
        (state) => {
          state match {
            //Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
            case Machine(true, candies, coins) if candies > 0 => ((candies, coins), Machine(false, candies, coins + 1))
            //Any other case the machine remain will be locked state and coin increase
            case Machine(locked, candies, coins) => ((candies, coins), Machine(locked, candies, coins + 1))
          }
        }
      case Turn =>
        (state) => {
          state match {
            //turning the knob on an unlocked machine gives a candy and lock the machine
            case Machine(false, candies, coins) => ((candies, coins), Machine(true, candies - 1, coins))
            //turning the knob on a locked machine has no effect
            case machine@Machine(true, candies, coins) => ((candies, coins), machine)
          }
        }
    }
  )
  }
}





































