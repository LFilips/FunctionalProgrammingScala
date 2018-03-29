
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

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

//this will invert locked value and add one to candies and 2 coin coin
val state1: State[Machine,(Int,Int)] = State((state: Machine) => ((state.candies, state.coins), Machine(!state.locked, state.candies + 1, state.coins + 2)))

state1.run(Machine(true, 0, 1))

//this will invert locked value and add one two candies and 1 coin
val state2: State[Machine,(Int,Int)] = State((state: Machine) => ((state.candies, state.coins), Machine(!state.locked, state.candies + 2, state.coins + 1)))

//How i can compose this two operation together?

val run1: Machine => ((Int,Int),Machine) = state1.run
val run2: Machine => ((Int,Int),Machine) = state2.run


//the state output of one of the state need to be input for the other
val composedFunction : Machine => ((Int,Int),Machine) = (state:Machine) => run2(run1(state)._2)


val composedState = State(composedFunction)

val completeComposition = State((state:Machine) => run2(run1(state)._2))

composedState.run(Machine(true, 0, 1))

completeComposition.run(Machine(true, 0, 1))






