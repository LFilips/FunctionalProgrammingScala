package chapter13

class ExternalEffectsIO {


  def separatingIO(): Unit = {


    //consider the case that we have a method that does some sort of io

    /**
      * The method is proclaming the winner of a contest
      *
      * @param p1Points
      * @param p2Points
      */
    def proclameWinner(p1Points: Int, p2Points: Int): Unit = {
      p1Points match {
        case p1Points if p1Points > p2Points => printf("Player 1 won!")
        case p1Points if p1Points < p2Points => printf("Player 2 won!")
        case _ => printf("Draw!")
      }
    }

    //this can be refactored in something that pushes the side effect as out as possible

    def proclameWinnerPure(p1Points: Int, p2Points: Int): String = {
      p1Points match {
        case p1Points if p1Points > p2Points => "Player 1 won!"
        case p1Points if p1Points < p2Points => "Player 2 won!"
        case _ => "Draw!"
      }
    }

    //now the io is only present in this method call
    def proclameWinnerImpure(p1Points: Int, p2Points: Int) = {
      printf(proclameWinnerPure(p1Points, p2Points))
    }
  }


  /**
    * After that i've defined my trait for IO I can define a mehod that uses that dor doing the io
    */

  def printLine(line: String): IO = {
    new IO {
      override def run: Unit = println(line)
    }
  }

  /**
    * In this way we have an input and output, we are still not having any side effect
    * @param stringToPrint
    * @return
    */
  def printSomethingWithIoTrait(stringToPrint: String): IO = {
    printLine(stringToPrint) //this will not actually print anyThing, is just creating the io read for printingIt
  }

}


trait IO {
  def run: Unit
}
