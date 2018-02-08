package chapter1

object FirstChapterExercise {

  def fib(n: Int): Int = {
    require(n >= 0, "The index must be positive") //todo this will raise an exception, should be better to use option
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)
    go(n, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = as match {
    case Array() => true
    case Array(_) => true
    case Array(first,second,_*) => ordered(first,second) && isSorted(as.drop(1),ordered)
  }


}
