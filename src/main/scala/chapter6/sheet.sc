import chapter6.SimpleRNG


val randomNumber = scala.util.Random

randomNumber.nextDouble
randomNumber.nextDouble

/**
  * The Random class has some internal state that is used for generating
  * the number
  *
  */


Int.MinValue
Int.MaxValue
val number = -6

val negation = -number

val x : Int = - (-2147483648)
val y : Int = - (Int.MaxValue)


val emptyList = Nil

1 :: Nil


val rng = SimpleRNG(20)
val nextList = SimpleRNG.ints(10)(rng)._1
val nextList2 = SimpleRNG.ints(10)(rng)._1


val listWithValue = List(1,2,3,4)

List.fill(4)(listWithValue.head)

