import chapter5.Stream


val funct: Int => Int = (x) => {
  println(x);
  x
}

List(1, 2, 3).map(funct).map(funct)
Stream(1, 2, 3).map(funct).map(funct).toList


/**
  * The interesting this is that in case of a list this print are: 1 2 3 1 2 3 in case of a stream is 1 1 2 2 3 3
  * so the stream is evaluating all the function on each element of the stream, whilst the list is calling the first
  * map on the whole list and then the second map on the second list
  * why this is happen??
  *
  * Let's start looking into the costructor that is called with the stream
  *
  * def apply[A](as: A*): Stream[A] =
  * if (as.isEmpty) empty
  * else cons(as.head, apply(as.tail: _*))
  *
  * where:
  *
  * def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
  * lazy val head = hd
  * lazy val tail = tl
  * Cons(() => head, () => tail)
  * }
  */

/**
  *
  * An important thing that is happening in the constructor is that the by-name parameter is store inside a lazy val
  * hence is not executed, and then it is lifted to a lambda fucntion without param that executed that
  *
  */

lazy val y = 3

val z = () => y

List(1,2).map(funct).map(funct)

Stream(1,2).map(funct).map(funct)

/**
  *
  * So in this way what is happening is that head and tail are function that will produce a value and they are not really evaluated
  * and this is the reason why i can define infinite stream without having overflow error
  *
  * So the Stream constructor became:
  *
  * Cons(() => 1,() => apply(2)).map(funct).map(funct)
  *
  * Because the construction itself of the stream doesn't include the genereation of all the element but only the funciton that will produce it
  * at this point the map fuction is applied, and it is defined in therms of foldRight:
  *
  *

   def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(head, tail) => f(head(), tail().foldRight(z)(f))
    case Empty => z
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))
  }

  1) f(head(), tail().foldRight(z)(f)) where: f is cons(mapFunction(a),b)
  2) cons(mapFunction(head()), (tail().foldRight(z)(f))) --> here the head() is evaluated and the map function applied
     while the tail will remain an unevaluated function
  3) at this point we call again the map function, but the behaviour is the same, the map is applied to the head
      and the rest of the list will be a sequence of function to apply
  */





