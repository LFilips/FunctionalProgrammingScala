package chapter6

import org.scalatest.{FlatSpec, Matchers}

class MachineTest extends FlatSpec with Matchers {

  behavior of "MachineTest"


  //case class State[S, +A](run: S => (A, S))
  //Machine(locked: Boolean, candies: Int, coins: Int)

  it should "unlock the machine with a coin" in {

    //Initial dispenser
    val initialState = Machine(true,10,0) //locked machine with 10 candies and 0 coins

    val input = List(Coin)

    val expectedState = Machine(false,10,1)
      //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(10,1)

  }

  it should "unlock the machine with a coin and do add another coin with the second" in {

    //Initial dispenser
    val initialState = Machine(true,10,0) //locked machine with 10 candies and 0 coins

    val input = List(Coin,Coin)

    val expectedState = Machine(false,10,2)
    val expectedValues = (10,2)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "give a candy if turn when unlocked" in {

    //Initial dispenser
    val initialState = Machine(false,10,0) //locked machine with 10 candies and 0 coins

    val input = List(Turn)

    val expectedState = Machine(true,9,0)
    val expectedValues = (9,0)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "ignore the turn when the machine is locked" in {

    val initialState = Machine(true,10,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Turn,Turn,Turn,Turn,Turn)

    val expectedState = Machine(true,10,0)
    val expectedValues = (10,0)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "give 2 candy and get 3 coins and finish unlocked with the sequence Coin,Turn,Coin,Turn,Coin" in {

    val initialState = Machine(true,10,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Coin,Turn,Coin,Turn,Coin)

    val expectedState = Machine(false,8,3)
    val expectedValues = (8,3)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "give 3 candy and get 3 coins and finish locked with the sequence Coin,Turn,Coin,Turn,Coin" in {

    val initialState = Machine(true,10,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Coin,Turn,Coin,Turn,Coin,Turn)

    val expectedState = Machine(true,7,3)
    val expectedValues = (7,3)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "give  candy and get 3 coins with the sequence Coin,Turn,Coin,Turn,Coin" in {

    val initialState = Machine(true,10,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Coin,Coin,Coin,Turn,Coin)

    val expectedState = Machine(false,9,4)
    val expectedValues = (9,4)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "not unlock the machine when inserted a coin but no candies are left" in {

    val initialState = Machine(true,0,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Coin)

    val expectedState = Machine(true,0,1)
    val expectedValues = (0,1)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "unlock and increase the coin with Coin,Coin,Coin" in {

    val initialState = Machine(true,1,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Coin,Coin,Coin)

    val expectedState = Machine(false,1,3)
    val expectedValues = (1,3)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)

  }

  it should "not unlock the machine when it runs out of candies" in {

    val initialState = Machine(true,2,0) //unlocked machine with 10 candies and 0 coins

    val input = List(Coin,Coin,Turn,Turn,Coin,Turn,Coin)

    val expectedState = Machine(true,0,4)
    val expectedValues = (0,4)
    //(state) => (Machine(true,10,0),(10,0)) //unlocked with 7 candies and 3 coins

    val currentState = State.simulateMachine(input).run(initialState)

    currentState._2 should be(expectedState)
    currentState._1 should be(expectedValues)


  }

}
